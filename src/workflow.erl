-module(workflow).

-export([create_workflow_instance/4,
         deploy_workflows/1
        ]).

create_workflow_instance(WorkflowKey, BpmnProcessId, Version, Payload) ->
  Request =   #{workflowKey => WorkflowKey,
                bpmnProcessId => BpmnProcessId,
                version => Version,
                payload => Payload},
  Response = gateway_protocol_gateway_client:create_workflow_instance(ctx:new(), Request),
  case Response of
    {ok, Result,_Metadata} ->
            Result;
    Error -> Error
  end.

deploy_workflows(Workflows) ->
    WorkflowObjects = lists:map(
                        fun({Name, Type, FilePath}) ->
                            {ok, Object} = get_workflow_object(Name, Type, FilePath),
                            Object
                        end, Workflows),
    Request = #{workflows => WorkflowObjects},
    Response = gateway_protocol_gateway_client:deploy_workflow(ctx:new(), Request),
    case Response of
        {ok, WorkflowMetadata, _GrpcMetadata} -> WorkflowMetadata;
        Error -> Error
    end.

%%====================================================================
%% Internal functions
%%====================================================================

get_workflow_object(Name, Type, FilePath) ->
    case file:read_file(FilePath) of
        {ok, Resource} ->
            {ok, #{name => Name, type => Type, definition => Resource}};
        {error, Reason} ->
            {error_reading_file, Reason}
    end.
