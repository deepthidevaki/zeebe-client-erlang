-module(client_test_SUITE).

%% common_test callbacks
-export([ init_per_suite/1,
          end_per_suite/1,
          init_per_testcase/2,
          end_per_testcase/2,
          all/0
        ]).

%% Test cases
-export([workflow_instance_create_test/1,
         deploy_workflow_test/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%Add all testcases here
all() ->
 [workflow_instance_create_test,
 deploy_workflow_test].

 init_per_suite(Config) ->
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

deploy_workflow_test(_Config) ->
    Result = zbclient:deploy_workflow(
                    "order-process",'BPMN',
                    "/home/deepthi/work/zeebe/zeebe-broker-0.14.0/bin/order-process.bpmn"
            ),
    {ok, [Workflow]} = zbclient:get_workflows(Result),
    {ok, ProcessId} = zbclient:get_bpmn_process_id(Workflow),
    ?assertEqual(<<"order-process">>, ProcessId),
    {ok, Version} = zbclient:get_version(Workflow),
    ?assert(Version > 0),
    {ok, WorkflowKey} = zbclient:get_workflow_key(Workflow),
    CreateInstanceResponse = zbclient:create_workflow_instance(WorkflowKey, ProcessId, Version, "{\"orderId\": 12345}"),
    ?assertNotMatch({error, _}, CreateInstanceResponse),
    ?assertEqual({ok, ProcessId}, zbclient:get_bpmn_process_id(CreateInstanceResponse)),
    ?assertEqual({ok, WorkflowKey}, zbclient:get_workflow_key(CreateInstanceResponse)),
    ok.

workflow_instance_create_test(_Config) ->
    Result = zbclient:create_workflow_instance(3, "test-process"),
    %% #{bpmnProcessId := <<"test-process">>,version := _V,
    %%         workflowInstanceKey := _Key, workflowKey := 3} = Result,
    ?assertEqual({ok,<<"test-process">>}, zbclient:get_bpmn_process_id(Result)),
    ?assertEqual({ok, 3}, zbclient:get_workflow_key(Result)),
    ok.
