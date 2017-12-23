%%% Main entrypoint into compiler
-module(jarlang).
-author(["Chris Bailey", "Andrew Johnson"]).
-vsn(1.0).

-export([main/0,
         pipeline/1,
         pipeline/2]).

-include_lib("eunit/include/eunit.hrl").

%% Command line script entrypoint
%% Looks at arguments and based on arguments either performs transpilation of Erlang
%% source files to JavaScript, or runs tests such as eunit, xref...
%% TODO: 
%%      - Instead of using maps:get to talk to Args, we should write a argument parsing
%%        library
main() ->
    Args = parse_args(init:get_plain_arguments()),
    case maps:get(mode, Args) of
        "transpile" ->
            case length(maps:get(files, Args)) of
                0 ->
                    io:format("usage: jarlang.sh filename.erl filename2.erl ... filenameN.erl~n~n");
                _ ->
                    Self = self(),
                    EscodegenDir = maps:get(escodegen, Args),

                    % Spawn transpilation processes for all files given as arguments
                    % And create recieve blocks for all created processes
                    Pids = [spawn_transpilation_process(File, Self, EscodegenDir) || File <- maps:get(files, Args)],
                    [receive {Pid, Result} -> Result end || Pid <- Pids]
            end;
        "eunit" ->
            ModulesToTest = [
                asttrans,
                coregen,
                esast,
                jarlang,
                filepath,
                tokdata
            ],
            [run_tests(Module) || Module <- ModulesToTest]
    end,
    halt().

%% Spawns a process which performs the transpilation process for any given file.
%% Returns output to the spawning process
spawn_transpilation_process(File, MainProcess, EscodegenDir) ->
    spawn_link(fun() -> MainProcess ! {self(), esast:test(pipeline(js_ast, File), EscodegenDir)} end).

%% Synchronously performs all types of testing on a given module
%% Output is printed via io:format instead of returned
run_tests(Module) ->
    % Perform XREF Analysis for module
    io:format("Running XREF analysis for module '~p'~n", [Module]),
    [{_, DeprecatedFuns}, {_, UndefFuns}, _] = xref:m(Module),
    case {length(DeprecatedFuns), length(UndefFuns)} of
        {0, 0} ->
            io:format("  No Undefined or Deprecated functions used in module '~p'~n", [Module]);
        _ ->
            lists:map(fun({Func, Arity}) ->
                io:format("  Deprecated Function '~p/~p' used in module '~p'~n", [Func, Arity, Module])    
            end, DeprecatedFuns),
            lists:map(fun({Func, Arity}) ->
                io:format("  Undefined Function '~p/~p' used in module '~p'~n", [Func, Arity, Module])    
            end, UndefFuns)
    end,        

    % Perform EUNIT testing for module    
    io:format("Running any and all tests in module '~p'~n", [Module]),    
    eunit:test(Module).

%% Shorthand for calling pipeline/2 where the function called is pipeline going as far as
%% the pipeline is currently implemented.
pipeline(Module) ->
    pipeline(js, Module).

%% Calls different modules, all of which implement different parts of our compiler pipeline,
%% and returns the result of that stage of the pipeline to the calling function.
%% The first parameter should be an atom representing what stage of the pipeline you would like
%% to run and produce, such as 'core' or 'core_ast' to generate core_erlang or 
%% a core_erlang_ast respectively.
pipeline(core, Module) ->
    {ok, _ModuleName, BinaryData} = coregen:to_core_erlang(Module, return),
    BinaryData;
pipeline(core_ast, Module) ->
    {ok, AST} = coregen:to_core_erlang_ast(Module, return),
    AST;
pipeline(js_ast, Module) ->
    AST = pipeline(core_ast, Module),
    asttrans:erast2esast(AST);
pipeline(js, Module) ->
    AST = pipeline(js_ast, Module),
    esast:test(AST).

%% Parses the results of init:get_plain_args and returns a map of everything we expect, keyed
%% by either:
%%    - files, for all arguments which don't fall into the following categories
%%    - escodegen, an argument immediately following the flag '-escodegen'
%%    - mode, an argument immediately following the flag '-mode'
parse_args(Args) ->
    parse_args(Args, #{files => [], escodegen => null, mode => "transpile"}, null).

%% Return after parse complete
parse_args([], ArgsMap, _Prev) ->
    ArgsMap;

%% Adds to 'mode' key
parse_args(["-mode" | Args], ArgsMap, _Prev) ->
    parse_args(Args, ArgsMap, mode);
parse_args([Arg | Args], ArgsMap, mode) ->
    parse_args(Args, maps:update(mode, Arg, ArgsMap), null);

%% Adds to 'escodegen' key
parse_args(["-escodegen" | Args], ArgsMap, _Prev) ->
    parse_args(Args, ArgsMap, escodegen);
parse_args([Arg | Args], ArgsMap, escodegen) ->
    parse_args(Args, maps:update(escodegen, Arg, ArgsMap), null);

%% Adds to 'files' key
parse_args([Arg | Args], ArgsMap, _Prev) ->
    parse_args(Args, maps:update(files, [Arg | maps:get(files, ArgsMap)], ArgsMap), Arg).
