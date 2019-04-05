-module(job_handler).
-behaviour(gen_statem).

%% Process that polls for maxJobsToActivate jobs

-export([start_link/5,
        activate_jobs/5]).

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

activate_jobs(Type, Worker, Timeout, MaxJobsToActivate, HandlerFunction) ->
    start_link(Type, Worker, Timeout, MaxJobsToActivate, HandlerFunction).

start_link(Type, Worker, Timeout, MaxJobsToActivate, HandlerFunction) ->
    gen_statem:start_link(?MODULE, [Type, Worker, Timeout, MaxJobsToActivate, HandlerFunction], []).

init([Type, Worker, Timeout, MaxJobsToActivate, HandlerFunction]) ->
    Data = #{type => Type, worker => Worker, timeout => Timeout, maxJobsToActivate => MaxJobsToActivate,
             processedJobCount => 0, jobs => [], stream => undefined, jobHandler => HandlerFunction},
    {ok, initialize, Data, [{state_timeout, 0, poll}]}.

callback_mode() ->
    state_functions.

initialize_stream(#{type := Type, worker := Worker, timeout := Timeout, maxJobsToActivate := MaxJobsToActivate}) ->
    {ok, Stream} = gateway_protocol_gateway_client:activate_jobs(ctx:new(), #{type => Type,
                               worker => Worker,
                               timeout => Timeout,
                               maxJobsToActivate => MaxJobsToActivate}),
    grpcbox_client:recv_headers(Stream),
    {ok, Stream}.

initialize(state_timeout, _initialize, Data) ->
    {ok, Stream} = initialize_stream(Data),
    {next_state, ready, Data#{stream := Stream}, [{state_timeout, 1000, poll}]};

initialize(info, SomeInfo, _Data) ->
    io:format("Receives some info ~p ~n", [SomeInfo]),
    {keep_state_and_data, [{state_timeout, 1000, initialize}]};

initialize(_, Info, _Data) ->
    io:format("Received Unknown ~p~n", [Info]),
    {keep_state_and_data, [{state_timeout, 1000, initialize}]}.

ready(info, {data, Id, #{jobs := Jobs}}, Data) ->
        io:format("Received jobs ~n"),
        {next_state, process_jobs, Data#{jobs := Jobs}, [{state_timeout, 0, process}]};

ready(info, {'DOWN', _Ref, process, _Pid, _Reason}, _Data) ->
     {keep_state_and_data};

ready(info, {trailers, Id, _}, Data) ->
     {next_state, initialize, Data, [{state_timeout, 1000, initialize}]};

 ready(_, Info, Data) ->
     io:format("Received Unknown ~p~n", [Info]),
     {keep_state_and_data}.

process_jobs(state_timeout, _process, #{jobs := Jobs, processedJobCount := JobCount, jobHandler := JobHandler} = Data) ->
    lists:map(fun(Job) -> process(Job, JobHandler) end, Jobs),
    {next_state, ready, Data#{jobs := [], processedJobCount := JobCount + length(Jobs)}}.

process(Job, JobHandler) ->
    io:format("Processing job ~n"),
    JobHandler(Job).

handle_common(_, _, _) ->
    io:format("received unknown"),
    {keep_state_and_data, [{state_timeout, 0, something}]}.

terminate(_Reason, _State, #{processedJobCount := JobCount}) ->
    io:format("Stopping job handler after processing ~p processes~n", [JobCount]),
    ok.
