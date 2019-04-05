zeebe_client
=====

A client for [Zeebe](https://github.com/zeebe-io/zeebe).

This is a work in progress library. It doesn't support all operations yet.

Build
-----
Required: OTP 21.0

    $ rebar3 compile


Example script for erl shell
=====

Run zeebe broker version 0.17.0 on localhost:26500.

```
erl -pa ./_build/default/lib/*/ebin

application:load(grpcbox).
application:set_env(grpcbox, client, #{channels => [{default_channel, [{http, "localhost", 26500, []}], #{}}]}).
application:set_env(grpcbox, transport_opts, #{}).
application:ensure_all_started(grpcbox).

WorkflowDefinition = filename:join(code:priv_dir(zeebe_client), "order-process.bpmn").
zeebe_client:deploy_workflow("order-process",'BPMN',WorkflowDefinition).

zeebe_client:create_workflow_instance(2251799813685249, "order-process", 1, "{\"orderId\": 300}").

{ok, Handler} = zeebe_client:activate_jobs("payment-service", "test", 10000, 3,
	 fun(#{key := Key}) ->
		io:format("Processing job key ~p~n", [Key]),
		 zeebe_client.complete_job(Key, "{}")
	 end).

zeebe_client:cancel_job_handler(Handler).

```
