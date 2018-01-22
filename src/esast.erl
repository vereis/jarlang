%%% Module to help build valid JavaScript AST Nodes and trees
-module(esast).
-author(["Chris Bailey"]).
-vsn(1.0).

-compile(export_all).

%% Helper function to generate a equivalent c_module node in JS when given
%% the main attributes of a c_module node from a Core Erlang AST
c_module(ModuleName, ExportList, FunctionList) ->
    % Export list looks like [{"Funcname", Arity}...]
    % Function list looks like [{"Funcname/Arity", esast:functionDeclaration or esast:functionExpression}]
    % ?spec([{ModuleName, list}, {ExportList, list_of_tuple}, {FunctionList, list_of_tuple}]),
    estree:program([
        estree:const_declaration(
            list_to_binary(ModuleName),
            estree:call_expression(
                estree:function_expression(
                    null,
                    [],
                    estree:block_statement(
                        [
                            estree:use_strict(),
                            c_exports(ExportList),
                            c_functions(FunctionList),
                            estree:return_statement(
                                estree:identifier(<<"exports">>)
                            )
                        ]
                    ),
                    false
                ),
                []
            )
        )
    ]).

%% Eventually creates an export list which is intended to be returned in a c_module call.
%% Function arity is handled with switch/case statements and as we don't know how many different
%% versions of a function there are, we store a map of lists of functions to call. We then turn
%% each entry in the map into a function containing a switch/case statement on arg length.
c_exports(ExportList) ->
    MappedByFuncName = c_exports_mapfuncs(ExportList, #{}),
    estree:const_declaration(
        <<"exports">>,
        estree:object_expression(
            lists:map(fun({FuncName, Arities}) ->
                estree:property(
                    estree:literal(list_to_binary(FuncName)),
                    estree:function_expression(null,
                                              [],
                                              estree:block_statement(c_exports_gencases({FuncName, Arities})), false))
            end, maps:to_list(MappedByFuncName))
        )
    ).

%% Function which takes a list of tuples in the form {Key, Value} and generates a map in the
%% form #{Key => ListOfAllValuesBelongingToKey}
c_exports_mapfuncs([], Map) ->
    Map;
c_exports_mapfuncs([{FnName, FnArity} | Tails], Map) when is_list(FnName) , is_integer(FnArity) ->
    case maps:find(FnName, Map) of
        {ok, Val} ->
            NewVal = Val ++ [FnArity];
        _ ->
            NewVal = [FnArity]
    end,
    c_exports_mapfuncs(Tails, maps:put(FnName, NewVal, Map)).

%% Generates a switch statement for use in c_exports node. Takes a tuple representing function
%% name and arities belonging to said function name. Cases will be generated for all such arities
c_exports_gencases({FuncName, Arities}) when is_list(FuncName) ->
    [
        estree:switch_statement(
            estree:member_expression(estree:identifier(<<"arguments">>), estree:identifier(<<"length">>), false),
            lists:map(fun(Arity) ->
                estree:switch_case(
                    estree:literal(Arity),
                    [estree:return_statement(
                        estree:call_expression(
                            estree:member_expression(
                                estree:identifier(<<"functions">>),
                                estree:literal(iolist_to_binary([FuncName, "/", integer_to_binary(Arity)])),
                                true
                            ),
                            [estree:spread_element(estree:identifier(<<"arguments">>))]
                        )
                    ), estree:break_statement(null)]
                )
            end, Arities),
            false
        ),
        estree:error("exception error", "undefined function",
            estree:binary_expression(<<"+">>,
                estree:literal(list_to_binary(FuncName)),
                estree:binary_expression(<<"+">>,
                    estree:literal(<<"/">>),
                    estree:member_expression(estree:identifier(<<"arguments">>), estree:identifier(<<"length">>), false)
                )
            )
        )
    ].

%% Generates function datastructure which takes a list in the form of [{"SomeFun/2", FUNCTION}] where function
%% is an ESTree functionExpression / functionDeclaration and produces the following:
%% const functions = {"somefun/2": function() { ... }}
c_functions([{FuncNameWithArity, Function} | Rest]) ->
    FunctionList = [{FuncNameWithArity, Function} | Rest],
    estree:const_declaration(
        <<"functions">>,
        estree:object_expression(
            lists:map(fun({FuncName, FuncBody}) ->
                estree:property(
                    estree:literal(list_to_binary(FuncName)),
                    FuncBody
                )
            end , FunctionList)
        )
    ).