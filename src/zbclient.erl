-module(zbclient).

%% API exports
-export([create_workflow_instance/2]).

%%====================================================================
%% API functions
%%====================================================================

create_workflow_instance(WorkflowKey, BpmnProcessId) ->
  Request =   #{workflowKey => WorkflowKey,
                bpmnProcessId => BpmnProcessId,
                version => -1},
  Response = gateway_protocol_gateway_client:create_workflow_instance(ctx:new(), Request),
  case Response of
    {ok, Result,
          _Metadata} ->
            Result;
    Error -> Error
  end.

%%====================================================================
%% Internal functions
%%====================================================================
