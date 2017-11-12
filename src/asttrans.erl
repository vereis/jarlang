% Author: Andrew Johnson
% Walks down a CoreErlang AST and calls EStree.

-module(asttrans).
-export([erast2esast/1,
         readTokens/1]).

% Compiles a given Erlang source file to a EStree ast
erast2esast(AST) ->
    toksModule(AST).
    %esast:print(toksModule(AST)).

%Read the module token (first token)
toksModule({c_module, _A, {_, _, ModuleName}, Exports, _Attributes, Functions})->
	io:format("module: ~s ~n", [ModuleName]),
	io:format("    exports: ~p ~n", [tupleList_getVars_3(Exports)]),
	FormattedFunctions = toksFunctions(Functions),
    FormattedExports = lists:map(fun({N,A})->{atom_to_list(N),A} end,tupleList_getVars_3(Exports)),
    io:format("~p~n~n~n~n~n~n", [FormattedFunctions])
    esast:c_module(atom_to_list(ModuleName),FormattedExports,FormattedFunctions);
	
	
toksModule(T)->
	io:format("Unrecognised Token in module section: ~p", [T]).



%Split functions apart
toksFunctions(Functions)->
    lists:map(fun(X)->toksFunc(X) end,Functions).

%Read a function
toksFunc({{_, _, {FunctionName, Arity}}, {_, [compiler_generated], _, _}})->
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),esast:functionExpression(null,[],esast:blockStatement([esast:emptyStatement()]),false)};
toksFunc({{_, _, {FunctionName, Arity}}, {_, _, ParamNames, Body}})->
	%io:format("    function: ~s/~w ~n", [FunctionName, Arity]),
	%io:format("        parameters: ~p ~n", [tupleList_getVars_3(ParamNames)]),
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),esast:functionExpression(null,[],esast:blockStatement([toksFuncBody(Body)]),false)}.


%Parse the function body
toksFuncBody({c_call, _, {_, _, Module}, {_, _, FunctionName}, Params})->
	%io:format("        call function ~s:~s(", [Module, FunctionName]),
	%io:format("~p)~n", [tupleList_getVars_3(Params)]);
    io:format("",[]);
	
toksFuncBody({c_case, _, Condition, Clauses})->
	%io:format("        case statement:"),
	%toksFuncBody(Condition),
	%toksClauses(Clauses);
    io:format("",[]);
	
toksFuncBody({c_values, _, Values})->
	%io:format("~p~n", [tupleList_getVars_3(Values)]);
    io:format("",[]);

toksFuncBody({c_var, _, Var})->
	%io:format("~p~n",[Var]);
    io:format("",[]);
	
toksFuncBody({c_seq, _, A, B})->
	%toksFuncBody(A),
	%toksFuncBody(B);
    io:format("",[]);
	
toksFuncBody({c_let, _, [{_, _, Variable}], A, B})->
	%io:format("        Let statement: ~s~n", [Variable]),
	%toksFuncBody(A),
	%toksFuncBody(B);
    io:format("",[]);
	
% Is apply a local function call? Assignment from function? Assignment with pattern matching?
toksFuncBody({c_apply, _, {_,_,{FName,_Arity}}, Params})->
	%io:format("Call local function ~s(",[FName]),
	%io:format("~p)~n", [tupleList_getVars_3(Params)]);
    io:format("",[]);

toksFuncBody({c_apply, _, _A, _B})->
	%io:format("        Apply statement: ~n");
    io:format("",[]);
	
toksFuncBody({c_literal,_,Value})->
	io:format("        Literal ~p~n", [Value]),
    esast:returnStatement(esast:literal(Value));

toksFuncBody({c_tuple,_,Values})->
	%io:format("        Tuple ~p~n", [tupleList_getVars_3(Values)]);
    io:format("",[]);
    
toksFuncBody({c_primop,_,{_,_,Type},Details})->
    % io:format("        Error? ~p~n~p~n", [Type,Details]),
    esast:error(atom_to_list(Type),"TODO Errors dont parse nicely",esast:literal("Message"));
    % io:format("",[]);
    
    
toksFuncBody(T)->
    io:format("Unrecognised Token in function body: ~p", [T]).

%Loop through case clauses
toksClauses([])->ok;
toksClauses([{c_clause,_,MatchVals,_true,Body}|Rest])->
    % io:format("Clause:~p~n",[MatchVals]),
    toksFuncBody(Body),
    toksClauses(Rest).




tupleList_getVars_3([])->
    [];
tupleList_getVars_3([{_,_, Val} | Body])->
    [Val | tupleList_getVars_3(Body)];
tupleList_getVars_3([{_, _, Val, _} | Body])->
    [Val | tupleList_getVars_3(Body)].

readTokens([])->
    io:format("Done.",[]);
readTokens([T | Body])->
    io:format("~p\n", [T]),
    readTokens(Body).

rAtomToList([A|Rest])->
    [rAtomToList(A)|rAtomToList(Rest)];
rAtomToList({A})->
    {rAtomToList(A)};
rAtomToList({A,B})->
    {rAtomToList(A),rAtomToList(B)};
rAtomToList({A,B,C})->
    {rAtomToList(A),rAtomToList(B),rAtomToList(C)};
rAtomToList({A,B,C,D})->
    {rAtomToList(A),rAtomToList(B),rAtomToList(C),rAtomToList(D)};
rAtomToList(A) when is_atom(A) ->
    atom_to_list(A);
rAtomToList(A) when not is_atom(A) ->
    A.









% Currently unused so commenting out for compilation
%tuple_to_string(T) ->
%   lists:flatten(io:format("~p", [T])).
