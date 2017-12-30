%%% Module Description:
%%% Compiles Erlang Source code to Core Erlang
-module(coregen).
-author(["Chris Bailey", "Andrew Johnson"]).
-vsn(1.0).

-export([to_core_erlang/1,
         to_core_erlang/2,
         to_core_erlang_ast/1,
         to_core_erlang_ast/2]).

%% Compiles a given Erlang source file to a CoreErlang file, and writes the resultant
%% CoreErlang output to a file in the same directory as the given module
to_core_erlang(Module) ->
    to_core_erlang(Module, filepath:path(Module)).

%% Compiles a given Erlang source file to a CoreErlang file, and returns the raw result
%% to the caller of this function.
to_core_erlang(Module, return) ->
    compile:file(Module, [to_core, binary]);

%% Compiles a given Erlang source file to a CoreErlang file, and writes the resultant
%% CoreErlang output to the given OutputDirectory
to_core_erlang(Module, OutputDirectory) ->
    case re:run(OutputDirectory, ".*/$") of
        nomatch ->
            {error, output_directory_not_valid};
        _ ->
            compile:file(Module, to_core),

            % Compiling always generates output in working directory so lets
            % move it into the directory where source code exists
            FileName = filepath:name(Module),
            OldLocation = FileName ++ ".core",
            NewLocation = OutputDirectory ++ OldLocation,

            filepath:move(OldLocation, NewLocation),
            {ok, core_compiled}
    end.

%% Compiles a given Erlang source file to a CoreErlang AST file, and writes the resultant
%% CoreErlang AST output to a file in the same directory as the given module
to_core_erlang_ast(Module) ->
    to_core_erlang_ast(Module, filepath:path(Module)).

%% Compiles a given Erlang source file to a CoreErlang AST file, and returns the raw result
%% to the caller of this function.
to_core_erlang_ast(Module, return) ->
    ModuleName = filepath:name(Module),
    to_core_erlang(Module, "./"), % Write Core Erlang source for given module so we can
                                  % read it to scan and parse

    case file:read_file(ModuleName ++ ".core") of
        {ok, Bin} ->
            case core_scan:string(binary_to_list(Bin)) of
                {ok, Toks, _} ->
                    core_parse:parse(Toks);
                {error, E, _} ->
                    {error, {scan, E}}
            end;
        {error, E} ->
            {error, {read, E}}
    end;

%% Compiles a given Erlang source file to a CoreErlang AST file, and writes the resultant
%% CoreErlang AST output to a file in the given output directory
to_core_erlang_ast(Module, OutputDirectory) ->
    ModuleName = filepath:name(Module),
    case to_core_erlang_ast(Module, return) of
        {ok, AST} ->
            % Move compiled core erlang file to output directory
            OldLocation = ModuleName ++ ".core",
            NewLocation = OutputDirectory ++ OldLocation,
            filepath:move(OldLocation, NewLocation),

            % Write AST
            file:write_file(OutputDirectory ++ ModuleName ++ ".ast",
                            lists:flatten(io_lib:format("~p", [AST]))),

            {ok, ast_compiled};
        {error, E} ->
            {error, E}
    end.