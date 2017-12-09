%%% Module to help build valid JavaScript AST Nodes and trees
-module(esast).
-author(["Chris Bailey"]).
-vsn(1.0).

-compile(export_all). 
-compile({no_auto_import, [node/0]}).

-include_lib("eunit/include/eunit.hrl").
-include("estree/estree.hrl").

%% Helper function to generate a equivalent c_module node in JS when given
%% the main attributes of a c_module node from a Core Erlang AST
c_module(ModuleName, ExportList, FunctionList) ->
    % Export list looks like [{"Funcname", Arity}...]
    % Function list looks like [{"Funcname/Arity", esast:functionDeclaration or esast:functionExpression}]
    ?spec([{ModuleName, list}, {ExportList, list_of_tuple}, {FunctionList, list_of_tuple}]),   
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

%% Eventually creates an export list which is intended to be returned in a c_module call.
%% Function arity is handled with switch/case statements and as we don't know how many different
%% versions of a function there are, we store a map of lists of functions to call. We then turn
%% each entry in the map into a function containing a switch/case statement on arg length.
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
                            [esast:spreadElement(identifier(<<"arguments">>))]
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

%% Generates function datastructure which takes a list in the form of [{"SomeFun/2", FUNCTION}] where function
%% is an ESTree functionExpression / functionDeclaration and produces the following:
%% const functions = {"somefun/2": function() { ... }}
c_functions([{FuncNameWithArity, Function} | Rest]) ->
    FunctionList = [{FuncNameWithArity, Function} | Rest],
    constDeclaration(
        <<"functions">>,
        objectExpression(
            lists:map(fun({FuncName, FuncBody}) ->
                property(
                    literal(list_to_binary(FuncName)), 
                    FuncBody
                )
            end , FunctionList)
        )
    ).

%%% Misc Functions
test(Map) ->
    test(Map, "codegen.js").

test(Map, Escodegen) ->
    code:add_path("lib/"),
    Json = jsone:encode(Map),

    % codegen.js has to take a file since its non-trivial parsing JSON on the command line
    % and different shells may react differently, so we write out to a temp file and delete it
    % afterwards
    Temp = "temp.estreejson",
    filepath:write(Json, Temp),
    io:format("~p~n", [file:get_cwd()]),
    % We generally don't care what happens here, both cases ensure dir exists
    try io:format("~s", [os:cmd("node " ++ Escodegen ++ " " ++ Temp)]) of
        _ -> 
            filepath:delete(Temp),
            ok
    catch
        _ -> 
            filepath:delete(Temp),
            {err, "codegen.js failed for some reason"}
    end.