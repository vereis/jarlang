% Author: Andrew Johnson
% Converts a CoreErlang AST into a ESTree AST.

-module(asttrans).
-export([erast2esast/1]).

% Compiles a given Erlang source file to a EStree ast
erast2esast(AST) ->
	erTok2esTok(AST).

erTok2esTok({c_module,A,{_,_,ModName},Exports,Attributes,Functions})->
	io:format("module:~s~n",[ModName]),
	io:format("    exports:~p ~n",[tupleList_getVars_3(Exports)]),
	fnsTok2esTok(Functions);
	
	
	
erTok2esTok(T)->
	io:format("Unrecognised Token: ~s",[T]).


fnsTok2esTok([F|Body])->
	fnTok2esTok(F),
	fnsTok2esTok(Body);
fnsTok2esTok([])->
	ok.

fnTok2esTok({{_,_,{FName,ParamCount}},{_,_,ParamNames,Body}})->
	io:format("    function:~s/~w ~n",[FName,ParamCount]),
	io:format("        parameters:~p ~n",[tupleList_getVars_3(ParamNames)]).




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
