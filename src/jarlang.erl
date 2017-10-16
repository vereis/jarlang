% Author: Chris Bailey & Andrew Johnson
% Transpiles erlang to javascript

-module(jarlang).
-export([er2/2,
		 er2file/2,
		 er2file/3]).

% Transpiles a module through the pipeline as far as is implemented
er2(best,Module) ->
	er2(core_erlang,Module),
	AST=er2(core_AST,Module),
	asttrans:erast2esast(AST);


er2(core_erlang,Module)->
	coregen:er2ce(Module);


er2(core_AST,Module)->
	{ok,AST}=coregen:er2ast(Module,return_AST),
	AST.

%############################################

er2file(Task,Module)->
	code:add_path("../lib/"),
	OutputDirectory=filepath:path(Module),
	io:format(OutputDirectory++"~n",[]),
	er2file(Task,Module,OutputDirectory).
er2file(Task,Module,OutputDirectory)->
	toFile(Task,OutputDirectory,Module).


toFile(core_erlang,OutputDirectory,Module)->
	toFile(core_erlang,OutputDirectory,Module,"core");
toFile(core_AST,OutputDirectory,Module)->
	toFile(core_AST,OutputDirectory,Module,"ast").

toFile(Task,OutputDirectory,Module,Ext)->
	code:add_path("../lib/"),
	ModuleName = filepath:name(Module),
	Tuple=er2(Task,Module),
	Str=OutputDirectory ++ ModuleName ++ "." ++ Ext,
	file:write_file(Str, tuple_to_string(Tuple)).



% Surprisingly, no tuple_to_string functions exist as a BIF
tuple_to_string(T) ->
	lists:flatten(io_lib:format("~p", [T])).