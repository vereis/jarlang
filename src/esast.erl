-module(esast).
-compile(export_all). 
-compile({no_auto_import, [node/0]}).

-include("estree/estree.hrl").

% -- Functions --
c_module(ModuleName, [{FuncName, Arity} | OtherExports], 
         [{FuncNameWithArity, Function} | OtherFunctions]) when is_list(ModuleName) , is_list(FuncName) , is_integer(Arity) , is_list(FuncNameWithArity) ->
    ExportList = [{FuncName, Arity} | OtherExports],
    FunctionList = [{FuncNameWithArity, Function} | OtherFunctions],
    program([
        constDeclaration(
            list_to_binary(ModuleName),
            callExpression(
                functionExpression(
                    null,
                    [],
                    blockStatement(
                        [
                            useStrict(),
                            c_exports(ExportList),
                            c_functions(FunctionList),
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
    ]).

% Eventually creates an export list which is intended to be returned in a c_module call.
% Function arity is handled with switch/case statements and as we don't know how many different
% versions of a function there are, we store a map of lists of functions to call. We then turn
% each entry in the map into a function containing a switch/case statement on arg length.
c_exports(ExportList) ->
    MappedByFuncName = c_exports_mapfuncs(ExportList, #{}),
    constDeclaration(
        <<"exports">>, 
        objectExpression(
            lists:map(fun({FuncName, Arities}) ->
                property(
                    literal(list_to_binary(FuncName)), 
                    functionExpression(null, [], blockStatement(c_exports_gencases({FuncName, Arities})), false)
                )       
            end, maps:to_list(MappedByFuncName))                                 
        )
    ).

% Function which takes a list of tuples in the form {Key, Value} and generates a map in the
% form #{Key => ListOfAllValuesBelongingToKey}
c_exports_mapfuncs([], Map) ->
    Map;
c_exports_mapfuncs([{FnName, FnArity} | Tails], Map) when is_list(FnName) , is_integer(FnArity) ->
    case maps:find(FnName, Map) of
        {ok, Val} ->
            NewVal = Val ++ [FnArity];
        _else ->
            NewVal = [FnArity]
    end,
    c_exports_mapfuncs(Tails, maps:put(FnName, NewVal, Map)).

% Generates a switch statement for use in c_exports node. Takes a tuple representing function
% name and arities belonging to said function name. Cases will be generated for all such arities
c_exports_gencases({FuncName, Arities}) when is_list(FuncName) ->
    [
        switchStatement(
            memberExpression(identifier(<<"arguments">>), identifier(<<"length">>), false),
            lists:map(fun(Arity) ->
                switchCase(
                    literal(Arity),
                    [returnStatement(
                        callExpression(
                            memberExpression(
                                identifier(<<"functions">>), 
                                literal(iolist_to_binary([FuncName, "/", integer_to_binary(Arity)])), 
                                true
                            ),
                            [identifier(<<"arguments">>)]
                        )   
                    ), breakStatement(null)]     
                )       
            end, Arities),
            false
        ),        
        error("exception error", "undefined function", 
            binaryExpression(<<"+">>, 
                literal(list_to_binary(FuncName)),
                binaryExpression(<<"+">>,
                    literal(<<"/">>),
                    memberExpression(identifier(<<"arguments">>), identifier(<<"length">>), false)
                )
            )
        )
    ].

% Generates function datastructure which takes a list in the form of [{"SomeFun/2", FUNCTION}] where function
% is an ESTree functionExpression / functionDeclaration and produces the following:
% const functions = {"somefun/2": function() { ... }}
c_functions([{FuncNameWithArity, Function} | Rest]) ->
    FunctionList = [{FuncNameWithArity, Function} | Rest],
    constDeclaration(
        <<"functions">>,
        objectExpression(
            lists:map(fun({FuncName, _Fn}) ->
                property(
                    literal(list_to_binary(FuncName)), 
                    functionExpression(null, [], blockStatement([
                        % Function
                        returnStatement(literal(<<"function parsing not yet implemented">>))
                    ]), false)
                )
            end , FunctionList)
        )
    ).

% ------ Intenal ------ %

% Generates a plain node, setting only node type
node(Type) ->
    node(Type, []).

% Generates a plain node and sets additional fields in the form
% List[{Key, Value}]
node() ->
    #{}.

node(Type, AdditionalFields) when is_atom(Type) ->
    node(atom_to_binary(Type, "utf-8"), AdditionalFields);
node(Type, AdditionalFields) when is_list(Type) ->
    node(list_to_binary(Type), AdditionalFields);
node(Type, AdditionalFields) ->
    NewNode = #{"type" => Type},
    updateRecord(NewNode, AdditionalFields).

% Add location data to any node
addLocationData(Node, LineNumber, ColStart, ColEnd) ->
    updateRecord(Node, [{"loc", sourceLocation(LineNumber, ColStart, ColEnd)}]).

% Helper function which appends new key value pairs into an existing record
updateRecord(Record, [{Key, Value}]) ->
    Record#{Key => Value};
updateRecord(Record, [{Key, Value} | Tail]) ->
    updateRecord(Record#{Key => Value}, Tail).


% Misc Functions
test(Map) ->
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
