-module(job_handler).
-behaviour(gen_statem).

%% Process that polls for maxJobsToActivate jobs

-export([start_link/4,
        activate_jobs/4]).

-export([callback_mode/0,
         terminate/3,
         init/1,
         handle_common/3
         ]).

%% states
-export([ready/3,
         process_jobs/3,
         initialize/3
        ]).

activate_jobs(Type, Worker, Timeout, MaxJobsToActivate) ->
    start_link(Type, Worker, Timeout, MaxJobsToActivate).

start_link(Type, Worker, Timeout, MaxJobsToActivate) ->
    gen_statem:start_link(?MODULE, [Type, Worker, Timeout, MaxJobsToActivate], []).

init([Type, Worker, Timeout, MaxJobsToActivate]) ->
    Data = #{type => Type, worker => Worker, timeout => Timeout, maxJobsToActivate => MaxJobsToActivate,
             processedJobCount => 0, jobs => [], stream => undefined},
    {ok, Stream} = initialize_stream(Data),
    {ok, ready, Data#{stream := Stream}, [{state_timeout, 0, poll}]}.

callback_mode() ->
    state_functions.

initialize_stream(#{type := Type, worker := Worker, timeout := Timeout, maxJobsToActivate := MaxJobsToActivate}) ->
    {ok, Stream} = gateway_protocol_gateway_client:activate_jobs(ctx:new(), #{type => Type,
                               worker => Worker,
                               timeout => Timeout,
                               maxJobsToActivate => MaxJobsToActivate}),
    H = grpcbox_client:recv_headers(Stream),
    {ok, Stream}.

initialize(state_timeout, _initialize, Data) ->
    {ok, Stream} = initialize_stream(Data),
    {next_state, ready, Data#{stream := Stream}, [{state_timeout, 1000, poll}]};

initialize(info, _SomeInfo, _Data) ->
    {keep_state_and_data, [{state_timeout, 1000, poll}]}.

ready(state_timeout, _poll, #{stream := Stream} = Data) ->
    io:format("In ready ~n"),
    case grpcbox_client:recv_data(Stream) of
        {ok, #{jobs := Jobs}} ->
            io:format("Received jobs ~n"),
            {next_state, process_jobs, Data#{jobs := Jobs}, [{state_timeout, 0, process}]};
        timeout ->
            {keep_state_and_data, [{state_timeout, 1000, poll}]};
        stream_finished ->
            io:format("Stream finished ~n"),
            grpcbox_client:recv_trailers(Stream),
            {stop, normal}
    end.

process_jobs(state_timeout, _process, #{jobs := []} = Data) ->
    io:format("Job list empty ~n"),
    {next_state, ready, Data, [{state_timeout, 0, poll}]};
process_jobs(state_timeout, _process, #{jobs := [Job |Jobs], processedJobCount := JobCount } = Data) ->
    process(Job),
    {next_state, process_jobs, Data#{jobs := Jobs, processedJobCount := JobCount + 1}, [{state_timeout, 0, process}]};
process_jobs(state_timeout, _, Data) ->
    io:format("Just debugging ~p", [Data]).

process(Job) ->
    io:format("Processing job. ~p~n", [Job]).

handle_common(_, _, _) ->
    io:format("received unknown"),
    {keep_state_and_data, [{state_timeout, 0, something}]}.

terminate(_Reason, _State, #{processedJobCount := JobCount}) ->
    io:format("Stopping job handler after processing ~p processes~n", [JobCount]),
    ok.
