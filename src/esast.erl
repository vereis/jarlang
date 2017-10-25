-module(esast).
-compile(export_all). 
-compile({no_auto_import, [node/0]}).

% -- EStree definitions --
-include("estree_primitives.hrl").
-include("estree_statements.hrl").
-include("estree_declarations.hrl").
-include("estree_expressions.hrl").

% -- Functions --
c_module(ModuleName, ExportList, _FunctionList) ->
    program(
        constDeclaration(
            list_to_binary(ModuleName),
            callExpression(
                functionExpression(
                    null,
                    [],
                    blockStatement(
                        [
                            c_exports(ExportList),
                            constDeclaration(
                                <<"functions">>,
                                objectExpression([])
                            ),
                            returnStatement(
                                identifier(<<"exports">>)
                            )
                        ]
                    ),
                    false
                ),
                []
            )
        )
    ).

c_exports(ExportList) ->
    constDeclaration(<<"exports">>, objectExpression(c_exports(ExportList, []))).
c_exports([], Funcs) ->
    Funcs;
c_exports([{FnName, FnArity} | Tails], Funcs) ->
    FuncName = iolist_to_binary([list_to_binary(FnName), <<"/">>, list_to_binary([FnArity])]),
    Func = functionExpression(null, [], blockStatement(
        [
            returnStatement(
                callExpression(
                    memberExpression(
                        identifier(<<"functions">>),
                        literal(FuncName),
                        true
                    ),
                    identifier(<<"arguments">>)
                )
            )
        ]
    ), false),
    FuncContainer = property(
        literal(FuncName), Func),
    c_exports(Tails, Funcs ++ [FuncContainer]).



% ------ Intenal ------ %

% Generates a plain node, setting only node type
node(Type) ->
    node(Type, []).

% Generates a plain node and sets additional fields in the form
% List[{Key, Value}]
node() ->
    #{}.

node(Type, AdditionalFields) when is_atom(Type) ->
    node(atom_to_binary(Type, utf-8), AdditionalFields);
node(Type, AdditionalFields) when is_list(Type) ->
    node(list_to_binary(Type), AdditionalFields);
node(Type, AdditionalFields) ->
    NewNode = #{type => Type},
    updateRecord(NewNode, AdditionalFields).

% Add location data to any node
addLocationData(Node) ->
    updateRecord(Node, []).

% Helper function which appends new key value pairs into an existing record
updateRecord(Record, [{Key, Value}]) ->
    Record#{Key => Value};
updateRecord(Record, [{Key, Value} | Tail]) ->
    updateRecord(Record#{Key => Value}, Tail).


% Misc Functions
print(Map) ->
    code:add_path("../lib/"),
    Json = json:serialize(Map),

    % codegen.js has to take a file since its non-trivial parsing JSON on the command line
    % and different shells may react differently, so we write out to a temp file and delete it
    % afterwards
    Temp = "temp.estreejson",
    filepath:write(Json, Temp),
    % We generally don't care what happens here, both cases ensure dir exists
    try io:format("~s", [os:cmd("node codegen.js " ++ Temp)]) of
        _ -> 
            filepath:delete(Temp),
            ok
    catch
        _ -> 
            filepath:delete(Temp),
            {err, "codegen.js failed for some reason"}
    end.
