-module(client_test_SUITE).

%% common_test callbacks
-export([
          init_per_suite/1,
          end_per_suite/1,
          init_per_testcase/2,
          end_per_testcase/2,
         all/0,
         wf_instance_create_test/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
 [wf_instance_create_test].

 init_per_suite(Config) ->
    application:load(grpcbox),
    application:set_env(grpcbox, client, #{channels => [{default_channel, [{http, "localhost", 26500, []}], #{}}]}),
    application:set_env(grpcbox, transport_opts, #{}),
application:ensure_all_started(grpcbox),
    Config.

 end_per_suite(Config) ->
     Config.

 init_per_testcase(_Case, Config) ->
    application:set_env(grpcbox, client, #{channels => [{default_channel, [{http, "localhost", 26500, []}], #{}}]}),
    Config.

 end_per_testcase(_Name, _) ->
     ok.

wf_instance_create_test(_Config) ->
    Result = zbclient:create_workflow_instance(3, "test-process"),
    #{bpmnProcessId := <<"test-process">>,version := _V,
             workflowInstanceKey := _Key, workflowKey := 3} = Result,
    ok.
