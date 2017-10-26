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
	%io:format("module: ~s ~n", [ModuleName]),
	%io:format("    exports: ~p ~n", [tupleList_getVars_3(Exports)]),
	esast:print(esast:c_module(atom_to_list(ModuleName),[],"")),
	toksFunctions(Functions);
	
	
toksModule(T)->
	io:format("Unrecognised Token in module section: ~p", [T]).

%Split functions apart
toksFunctions([F | Body])->
	toksFunc(F),
	toksFunctions(Body);
toksFunctions([])->
	ok.

%Read a function
toksFunc({{_, _, {FunctionName, Arity}}, {_, _, ParamNames, Body}})->
	io:format("    function: ~s/~w ~n", [FunctionName, Arity]),
	io:format("        parameters: ~p ~n", [tupleList_getVars_3(ParamNames)]),
	toksFuncBody(Body).

%Parse the function body
toksFuncBody({c_call, _, {_, _, Module}, {_, _, FunctionName}, Params})->
	io:format("        call function ~s:~s(", [Module, FunctionName]),
	io:format("~p)~n", [tupleList_getVars_3(Params)]);
	
toksFuncBody({c_case, _, Condition, Clauses})->
	io:format("        case statement:"),
	toksFuncBody(Condition),
	toksClauses(Clauses);
	
toksFuncBody({c_values, _, Values})->
	io:format("~p~n", [tupleList_getVars_3(Values)]);

toksFuncBody({c_var, _, Var})->
	io:format("~p~n",[Var]);
	
toksFuncBody({c_seq, _, A, B})->
	toksFuncBody(A),
	toksFuncBody(B);
	
toksFuncBody({c_let, _, [{_, _, Variable}], A, B})->
	io:format("        Let statement: ~s~n", [Variable]),
	toksFuncBody(A),
	toksFuncBody(B);
	
% Is apply a local function call? Assignment from function? Assignment with pattern matching?
toksFuncBody({c_apply, _, {_,_,{FName,_Arity}}, Params})->
	io:format("Call local function ~s(",[FName]),
	io:format("~p)~n", [tupleList_getVars_3(Params)]);

toksFuncBody({c_apply, _, _A, _B})->
	io:format("        Apply statement: ~n");
	
toksFuncBody({c_literal,_,Value})->
	%io:format("        Literal ~p~n", [Value]);
	esast:print(esast:literal(Value));

toksFuncBody({c_tuple,_,Values})->
	io:format("        Tuple ~p~n", [tupleList_getVars_3(Values)]);
	
toksFuncBody(T)->
	io:format("Unrecognised Token in function body: ~p", [T]).

%Loop through case clauses
toksClauses([])->ok;
toksClauses([{c_clause,_,MatchVals,_true,Body}|Rest])->
	io:format("Clause:~p~n",[MatchVals]),
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

% Currently unused so commenting out for compilation
%tuple_to_string(T) ->
%	lists:flatten(io:format("~p", [T])).
