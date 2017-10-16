% Author: Chris Bailey & Andrew Johnson
% Has the ability to compile Erlang into CoreErlang and also compile
% from Erlang straight to a CoreErlang AST.

-module(coregen).
-export([er2best/1,
		 er2ce/1,
         er2ce/2,
         er2ast/1,
         er2ast/2]).

er2best(Module) ->
	{ok,AST}=er2ast(Module, return_AST),
	asttrans:erast2esast(AST).

% Compiles a given Erlang source file to a CoreErlang file
er2ce(Module) ->
    code:add_path("../lib/"),
    er2ce(Module, filepath:path(Module)).

er2ce(Module, OutputDirectory) ->
    case re:run(OutputDirectory, ".*/$") of
        nomatch ->
            {error, output_directory_not_valid};
        _ ->
            code:add_path("../lib/"),
            compile:file(Module, to_core),

            % Compiling always generates output in working directory so lets
            % move it into the directory where source code exists
            FileName = filepath:name(Module),
            filepath:move(FileName ++ ".core", OutputDirectory ++ FileName ++ ".core"),
            {ok, core_compiled}
    end.

% Compiles a given Erlang source file to a CoreErlang AST file
er2ast(Module) ->
    code:add_path("../lib/"),
    er2ast(Module, filepath:path(Module)).

er2ast(Module, return_AST) ->
    code:add_path("../lib/"),
    er2ce(Module, "./"),

    % Parse core erlang and generate AST, credits to
    % http://www.robertjakob.de/posts/ceast.html :)
    % for general method to generate AST
    ModuleName = filepath:name(Module),
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


er2ast(Module, OutputDirectory) ->
    code:add_path("../lib/"),
    ModuleName = filepath:name(Module),
	case er2ast(Module, return_AST) of
		{ok, AST} ->
			% Move file to wherever user specified. This has an aftereffect of ensuring dir exists
			% so we can write the AST straight to directory
			filepath:move(ModuleName ++ ".core", OutputDirectory ++ ModuleName ++ ".core"),
			file:write_file(OutputDirectory ++ ModuleName ++ ".ast", tuple_to_string(AST)),
			{ok, ast_compiled};
		{error, E} ->
			{error, E}
	end.


% Surprisingly, no tuple_to_string functions exist as a BIF
tuple_to_string(T) ->
    lists:flatten(io_lib:format("~p", [T])).
