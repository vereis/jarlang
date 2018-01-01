%%% Module Description:
%%% Main entrypoint into compiler
-module(jarlang).
-author(["Chris Bailey", "Andrew Johnson"]).

-define(VERSION, "2.1.0").

-vsn(?VERSION).

%%% Export all functions if we compiled with erlc -dTEST or c(?MODULE, {d, 'TEST'}).
%%% This is so that we can run external eunit tests
-ifdef(TEST).
    -compile(export_all).
-else.
    -export([main/1,
            pipeline/1,
            pipeline/2]).
-endif.

%%% ---------------------------------------------------------------------------------------------%%%
%%% - PUBLIC FUNCTIONS --------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%%% Define the arguments that this program can take
-define(DEFAULT_ARGS, [
    {["-o", "--output"], o_output, singleton, ".", "Sets the output directory for compiled code. " ++
                                                       "If the directory doesn't exist, we will create it."},

    {["-t", "--type"], o_type, singleton, "js", "Sets the output type, defaults to 'js'. " ++
                                                       "Valid options are 'js', 'js_ast', 'core_ast', 'core' & 'all'."},

    {["-h", "--help"], o_help, is_set, false, "Displays this help message and exits."},

    {["-v", "--version"], o_vsn, is_set, false, "Displays current build version."}
]).

%% Main entrypoint into Jarlang, parses given arguments and decides on what to do.
main(Args) ->
    % Normalize args to strings if they're atoms, and parse them.
    NArgs = [case is_atom(Arg) of true -> atom_to_list(Arg); false -> Arg end || Arg <- Args],
    ParsedArgs = pkgargs:parse(NArgs, ?DEFAULT_ARGS),

    % Read ParsedArgs for arguments
    ShowHelp = pkgargs:get(o_help, ParsedArgs),
    ShowVsn  = pkgargs:get(o_vsn, ParsedArgs),
    OutDir   = pkgargs:get(o_output, ParsedArgs),
    Type   = list_to_atom(pkgargs:get(o_type, ParsedArgs)),
    Files    = perform_wildcard_matches(pkgargs:get(default,  ParsedArgs)),

    try branch(ShowHelp, ShowVsn, OutDir, Type, Files) of
        _ -> ok
    catch
        throw:usage ->
            usage(),
            help();
        throw:help ->
            help();
        throw:vsn ->
            version();
        throw:{invalid_type, T} ->
            io:format("Type '~p' is an invalid output type. Aborting...~n~n", [T])
    end,

    % Clean up and stop
    init:stop().





%%% ---------------------------------------------------------------------------------------------%%%
%%% - ENTRYPOINT CODE ---------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Determines which function to run depending on arguments given.
branch(_Help = false, _Vsn = false, Outdir, all, Files) when (length(Files) > 0)->
    transpile(Files, Outdir, core),
    transpile(Files, Outdir, core_ast),
    transpile(Files, Outdir, js_ast),
    transpile(Files, Outdir, js);
branch(_Help = false, _Vsn = false, Outdir, Type, Files) when (length(Files) > 0) ->
    transpile(Files, Outdir, Type);
branch(_Help = true, _Vsn = false, _OutDir, _Type, _Files = []) ->
    throw(help);
branch(_Help = false, _Vsn = true, _OutDir, _Type, _Files = []) ->
    throw(vsn);
branch(_Help, _Vsn, _OutDir, _Type, _Files) ->
    throw(usage).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - COMPILER PIPELINE -------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Convinience function for asynchronously transpiling erl files into js files, and synchronously
%% returning after all processes finish.
%% Also sets up compilation environment by extracting files to a tmp directory. Once transpilation
%% finishes, it then cleans up said directory
transpile(Files, Outdir, js) ->
    Codegen = pkgutils:pkg_extract_file("codegen.js"),
              pkgutils:pkg_extract_dir("node_modules.zip"),
    Pids = [spawn_transpilation_process(File, js_ast, self()) || File <- Files],

    %% Pass transpilated results into codegen.js to get JavaScript which we can write to a file
    [
        receive {Pid, {Module, Ast}} ->
            gen_js(Ast, Module, Codegen, Outdir)
        end
        || Pid <- Pids
    ],
    pkgutils:pkg_clean_tmp_dir();

%% Convinience function for asynchronously transpiling erl files into core_ast files, and synchronously
%% returning after all processes finish.
transpile(Files, Outdir, Type) ->
    ok = is_valid_type(Type),
    Pids = [spawn_transpilation_process(File, Type, self()) || File <- Files],

    % Write results to file
    [
        receive {Pid, {Module, Data}} ->
            write_other(io_lib:format("~p", [Data]), Module, Type, Outdir)
        end
        || Pid <- Pids
    ].

%% Spawns a process which performs transpilation for any given file.
%% Returns output to the spawning process
spawn_transpilation_process(File, Type, MainProcess) ->
    spawn_link(fun() ->
        MainProcess ! {self(), pipeline(Type, File)}
    end).

%% Shorthand for calling pipeline/2 where the function called is pipeline going as far as
%% the pipeline is currently implemented.
pipeline(Module) ->
    pipeline(js_ast, Module).

%% Calls different modules, all of which implement different parts of our compiler pipeline,
%% and returns the result of that stage of the pipeline to the calling function.
%% The first parameter should be an atom representing what stage of the pipeline you would like
%% to run and produce, such as 'core' or 'core_ast' to generate core_erlang or
%% a core_erlang_ast respectively.
pipeline(core, Module) ->
    {ok, _ModuleName, BinaryData} = coregen:to_core_erlang(Module, return),
    {Module, BinaryData};
pipeline(core_ast, Module) ->
    {ok, AST} = coregen:to_core_erlang_ast(Module, return),
    {Module, AST};
pipeline(js_ast, Module) ->
    {_Module, AST} = pipeline(core_ast, Module),
    {Module, asttrans:erast2esast(AST)}.

%% Generate javascript by writing out a javascript AST to a file and passing it into codegen.js
%% Then proceeds to read the output of codegen.js and writes it to a file
gen_js(Ast, File, Codegen, Outdir) ->
    Json = jsone:encode(Ast),

    %% We need to write our json into a temp file so that we can easily pass it into codegen.js
    %% We'll dump the temp file into the same directory as codegen.js
    WorkingDirectory = filename:dirname(Codegen),
    Filename = filename:basename(filename:rootname(File)),
    TempFile = lists:flatten([WorkingDirectory, "/", Filename, ".json"]),
    filepath:write(Json, TempFile),

    %% Pass json file into escodegen to generate JavaScript
    try os:cmd("node " ++ Codegen ++ " " ++ TempFile) of
        Result ->
            write_js(Result, Filename, Outdir)
    catch
        E ->
            {err, E}
    end.

%% Write results of codegen.js to a file
write_js(Result, Filename, Outdir) ->
    write_file(Result, Filename, "js", Outdir).

write_other(Data, File, core_ast, Outdir) ->
    Filename = filename:basename(filename:rootname(File)),
    write_file(Data, Filename, "est", Outdir);

write_other(Data, File, js_ast, Outdir) ->
    Filename = filename:basename(filename:rootname(File)),
    write_file(Data, Filename, "jst", Outdir);

write_other(Data, File, core, Outdir) ->
    Filename = filename:basename(filename:rootname(File)),
    write_file(Data, Filename, "core", Outdir).

%% Write results of codegen.js to a file
write_file(Result, Filename, _Ext, "io") ->
    io:format("==> Result of transpilation for: ~s.erl~n~s~n", [Filename, Result]);
write_file(Result, Filename, Ext, Outdir) ->
    filelib:ensure_dir(Outdir),
    ok = file:write_file(lists:flatten([Outdir, "/", Filename, ".", Ext]), Result).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - MISC FUNCTIONS ----------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Displays usage information
usage() ->
    SelfName = pkgutils:pkg_name(),
    io:format("Usage: ~s FILEs... [-o <dirname>][-t <type>]~n" ++
              "Compiles the Erlang source files FILEs you specify into JavaScript.~n" ++
              "Or if Type is set will output a file from that point in the pipeline.~n" ++
              "Example: ~s src/*.erl -o js/~n~n",
              [SelfName,
               SelfName]).

%% Displays help information
help() ->
    io:format("Valid Configuration Parameters:~n" ++
              "~s~n",
              [pkgargs:create_help_string(?DEFAULT_ARGS, 1, 55)]).

%% Displays version information
version() ->
    io:format("Current ~s version: v~s~n~n",
              [pkgutils:pkg_name(),
              ?VERSION]).

%% Looks through a list of file names and expands any wildcards that may exist.
%% None wildcards will just be added to the accumulator so that we can crash gracefully
%% later on.
perform_wildcard_matches([]) ->
    [];
perform_wildcard_matches(FileList) ->
    lists:foldl(fun(PotentialWildcard, Accumulator) ->
        case filelib:wildcard(PotentialWildcard) of
            [] ->
                Accumulator ++ [PotentialWildcard];
            Matches ->
                Accumulator ++ Matches
        end
    end, [], FileList).

%% Checks whether a given type is a valid one, and if so, returns ok.
%% Otherwise throws an error.
is_valid_type(String) when is_list(String) ->
    is_valid_type(list_to_atom(String));
is_valid_type(String) when is_binary(String) ->
    is_valid_type(binary_to_list(String));

is_valid_type(js) ->
    ok;
is_valid_type(js_ast) ->
    ok;
is_valid_type(core) ->
    ok;
is_valid_type(core_ast) ->
    ok;
is_valid_type(all) ->
    ok;
is_valid_type(Type) ->
    throw({invalid_type, Type}).