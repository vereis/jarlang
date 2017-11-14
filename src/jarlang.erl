% Author: Chris Bailey & Andrew Johnson
% Transpiles erlang to javascript

-module(jarlang).
-export([main/0,
         er2/2,
         er2file/2,
         er2file/3]).

% Main Function, Command line entrypoint
main() ->
    Args = parseArgs(init:get_plain_arguments()),
    case length(maps:get(files, Args)) of
        0 ->
            io:format("usage: jarlang.sh filename.erl filename2.erl ... filenameN.erl~n~n");
        X ->
            lists:map(fun(F) -> esast:test(er2(estree, F), maps:get(escodegen, Args)) end, maps:get(files, Args))
    end,
    halt().

% Transpiles a module through the pipeline as far as is implemented
er2(best,Module) ->
    er2(js,Module);

er2(core_erlang,Module)->
    {ok,_,CE}=coregen:er2ce(Module,return_CE),
    CE;

er2(core_AST,Module)->
    {ok,AST}=coregen:er2ast(Module,return_AST),
    AST;

er2(estree,Module)->
    AST=er2(core_AST,Module),
    asttrans:erast2esast(AST);

er2(js,Module)->
    esast:test(er2(estree,Module)).

parseArgs(Args) ->
    parseArgs(Args, #{files => [], escodegen => null}, null).

parseArgs([], ArgsMap, _Prev) ->
    ArgsMap;
parseArgs(["-escodegen" | Args], ArgsMap, _Prev) ->
    parseArgs(Args, ArgsMap, escodegen);
parseArgs([Arg | Args], ArgsMap, escodegen) ->
    parseArgs(Args, maps:update(escodegen, Arg, ArgsMap), null);
parseArgs([Arg | Args], ArgsMap, _Prev) ->
    parseArgs(Args, maps:update(files, [Arg | maps:get(files, ArgsMap)], ArgsMap), Arg).

%############################################

er2file(Task,Module)->
	code:add_path("lib/"),
	OutputDirectory=filepath:path(Module),
	er2file(Task,Module,OutputDirectory).
er2file(Task,Module,OutputDirectory)->
    toFile(Task,OutputDirectory,Module).


toFile(core_erlang,OutputDirectory,Module)->
    toFile(core_erlang,OutputDirectory,Module,"core");
toFile(core_AST,OutputDirectory,Module)->
    toFile(core_AST,OutputDirectory,Module,"ast");
toFile(estree,OutputDirectory,Module)->
    toFile(estree,OutputDirectory,Module,"est");
toFile(js,OutputDirectory,Module)->
    toFile(js,OutputDirectory,Module,"js").

toFile(Task,OutputDirectory,Module,Ext)->
	code:add_path("lib/"),
	ModuleName = filepath:name(Module),
	Tuple=er2(Task,Module),
	Str=OutputDirectory ++ ModuleName ++ "." ++ Ext,
	file:write_file(Str, tuple_to_string(Tuple)).



% Surprisingly, no tuple_to_string functions exist as a BIF
tuple_to_string(T) ->
    lists:flatten(io_lib:format("~p", [T])).
