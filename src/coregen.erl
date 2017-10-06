% Author: Chris Bailey
% Has the ability to compile Erlang into CoreErlang and also compile
% from Erlang straight to a CoreErlang AST.

-module(coregen).
-export([er2ce/1,
         er2ast/1,
         er2ast/2]).

% Compiles a given Erlang source file to a CoreErlang file
er2ce(Module) ->
    ModuleName = strip_extensions(Module, [".erl", ".beam", ".core", ".ast"]),
    compile:file(ModuleName ++ ".erl", to_core).

% Compiles a given Erlang source file to a CoreErlang AST file
er2ast(Module) ->
    ModuleName = strip_extensions(Module, [".erl", ".beam", ".core", ".ast"]),
    er2ce(ModuleName),

    % Parse core erlang and generate AST, credits to
    % http://www.robertjakob.de/posts/ceast.html :)
    % for general method to generate AST
    case file:read_file(ModuleName ++ ".core") of
        {ok, Bin} ->
            case core_scan:string(binary_to_list(Bin)) of
                {ok, Toks, _} ->
                    case core_parse:parse(Toks) of
                        {ok, AST} ->
                            file:write_file(ModuleName ++ ".ast", tuple_to_string(AST));
                        {error, E} ->
                            {error, {parse, E}}
                    end;
                {error, E, _} ->
                    {error, {scan, E}}
            end; 
        {error, E} ->          
            {error, {read, E}}
    end.

% When the argument to er2ast/2 is true, we delete the resulting .core file
% generated during AST compilation. Otherwise, behaves the same as erast(Module)
% or errors upon receiving an unknown argument.
er2ast(Module, false) ->
    er2ast(Module);
er2ast(Module, true) ->
    er2ast(Module),
    ModuleName = strip_extensions(Module, [".erl", ".beam", ".core", ".ast"]),
    file:delete(ModuleName ++ ".core");
er2ast(_Module, _OtherOption) ->
    {error, lists:flatten(
              io_lib:format("Unknown argument provided to func ~s:ce2ast/2", [?MODULE]))}.

% Strips given extensions from a given modulename
strip_extensions(ModuleName, Extensions) ->
    case re:replace(ModuleName, build_sanitation_regex(Extensions), "") of
        [Module,_Extension] ->
            binary_to_list(Module);
        Result ->
            Result
    end.

% Internally used by strip_extensions
build_sanitation_regex([Head | Tails]) ->
    build_sanitation_regex(Tails, "(" ++ Head ++ ")"). 

build_sanitation_regex([], ExtRegStr) ->
    ExtRegStr;
build_sanitation_regex([Head | Tails], ExtRegStr) ->
    build_sanitation_regex(Tails, ExtRegStr ++ "|(" ++ Head ++ ")"). 

% Surprisingly, no tuple_to_string functions exist as a BIF
tuple_to_string(T) ->
    lists:flatten(io_lib:format("~p", [T])).
