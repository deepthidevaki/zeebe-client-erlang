-module(client_test_SUITE).

%% common_test callbacks
-export([ init_per_suite/1,
          end_per_suite/1,
          init_per_testcase/2,
          end_per_testcase/2,
          all/0
        ]).

%% Test cases
-export([deploy_and_create_workflow_test/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%Add all testcases here
all() ->
 [deploy_and_create_workflow_test].

 init_per_suite(Config) ->
    %% TODO: start zeebe broker
    application:load(grpcbox),
    application:set_env(grpcbox, client, #{channels => [{default_channel, [{http, "localhost", 26500, []}], #{}}]}),
    application:set_env(grpcbox, transport_opts, #{}),
    application:ensure_all_started(grpcbox),
    Config.

 end_per_suite(Config) ->
     Config.

 init_per_testcase(_Case, Config) ->
    Config.

 end_per_testcase(_Name, _) ->
     ok.

deploy_and_create_workflow_test(_Config) ->
    Result = zbclient:deploy_workflow(
                    "order-process",'BPMN',
                    "/home/deepthi/work/zeebe/zeebe-broker-0.14.0/bin/order-process.bpmn"
            ),
    [Workflow] = record:get_workflows(Result),
    ProcessId = record:get_bpmn_process_id(Workflow),
    ?assertEqual(<<"order-process">>, ProcessId),
    Version = record:get_version(Workflow),
    ?assert(Version > 0),

    %% create a workflow instance
    WorkflowKey = record:get_workflow_key(Workflow),
    CreateInstanceResponse = zbclient:create_workflow_instance(WorkflowKey, ProcessId, Version, "{\"orderId\": 12345}"),
    ?assertNotMatch({error, _}, CreateInstanceResponse),
    ?assertEqual(ProcessId, record:get_bpmn_process_id(CreateInstanceResponse)),
    ?assertEqual(WorkflowKey, record:get_workflow_key(CreateInstanceResponse)),
    ok.
