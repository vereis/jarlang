% Author: Andrew Johnson
% Converts a CoreErlang AST into a ESTree AST.

-module(asttrans).
-export([erast2esast/1]).

% Compiles a given Erlang source file to a EStree ast
erast2esast(AST) ->
	toksModule(AST).

%Read the module token (first token)
toksModule({c_module,A,{_,_,ModName},Exports,Attributes,Functions})->
	io:format("module:~s~n",[ModName]),
	io:format("    exports:~p ~n",[tupleList_getVars_3(Exports)]),
	toksFunctions(Functions);
	
toksModule(T)->
	io:format("Unrecognised Token: ~s",[T]).

%Split functions apart
toksFunctions([F|Body])->
	toksFunc(F),
	toksFunctions(Body);
toksFunctions([])->
	ok.

%Read a function
toksFunc({{_,_,{FName,ParamCount}},{_,_,ParamNames,Body}})->
	io:format("    function:~s/~w ~n",[FName,ParamCount]),
	io:format("        parameters:~p ~n",[tupleList_getVars_3(ParamNames)]),
	toksFuncBody(Body).

%Parse the function body
toksFuncBody({c_call,_,{_,_,Module},{_,_,FName},Params})->
	io:format("        call function ~s:~s(",[Module,FName]),
	io:format("~p)~n",[tupleList_getVars_3(Params)]);
	
toksFuncBody({c_case,_,Condition,Clauses})->
	io:format("        case statement:"),
	toksCaseCond(Condition);
	
toksFuncBody({c_seq,_,A,B})->
	toksFuncBody(A),
	toksFuncBody(B);
	
toksFuncBody({c_let,_,[{_,_,Variable}],A,B})->
	io:format("        Let statement:~s~n",[Variable]),
	toksFuncBody(A),
	toksFuncBody(B);
	
toksFuncBody({c_apply,_,A,B})->
	io:format("        Apply statement:~n");
	
toksFuncBody(T)->
	io:format("Unrecognised Token: ~s",[T]).

%Convert the condition of a case statement
toksCaseCond({c_values,_,Values})->
	io:format("Values~n");
toksCaseCond({c_call,_,{_,_,Module},{_,_,FName},Params})->
	io:format("Call function ~s:~s(",[Module,FName]),
	io:format("~p)~n",[tupleList_getVars_3(Params)]).

% This breaks compilation - whats this meant to be?
%toksCaseCond({c_apply,_,A,B})->
%	io:format("Call function ~s:~s(",[Module,FName]),
%	io:format("~p)~n",[tupleList_getVars_3(Params)]).


tupleList_getVars_3([])->
	[];
tupleList_getVars_3([{_,_,Val}|Body])->
	[Val|tupleList_getVars_3(Body)];
tupleList_getVars_3([{_,_,Val,_}|Body])->
	[Val|tupleList_getVars_3(Body)].




readTokens([])->
	io:format("Done.",[]);
readTokens([T|Body])->
	io:format("~p\n", [T]),
	readTokens(Body).

tuple_to_string(T) ->
	lists:flatten(io:format("~p", [T])).
