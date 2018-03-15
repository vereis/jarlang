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
            pipeline/2,
            pipeline/3]).
-endif.





%%% ---------------------------------------------------------------------------------------------%%%
%%% - TYPE DEFINITIONS --------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

-type input_args() :: [atom() | string()].

-type pipeline_stage() :: js
                        | js_ast
                        | core
                        | core_ast
                        | all.

-type concurrency_mode() :: single_threaded
                          | multi_threaded.

%% Export concurrency_mode type since that'll be used in asttrans
-export_type([
    concurrency_mode/0
]).


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

    {["-v", "--version"], o_vsn, is_set, false, "Displays current build version."},

    {["--single-thread"], o_one_thread, is_set, false, "Forces transpilation to only use only one thread/process. " ++
                                                       "Single-threaded error output is also much nicer."}
]).

%% Main entrypoint into Jarlang, parses given arguments and decides on what to do.
-spec main(input_args()) -> ok.
main(Args) ->
    % Normalize args to strings if they're atoms, and parse them.
    NArgs = [case is_atom(Arg) of true -> atom_to_list(Arg); false -> Arg end || Arg <- Args],
    ParsedArgs = pkgargs:parse(NArgs, ?DEFAULT_ARGS),

    % Read ParsedArgs for arguments
    ShowHelp = pkgargs:get(o_help, ParsedArgs),
    ShowVsn  = pkgargs:get(o_vsn, ParsedArgs),
    OutDir   = pkgargs:get(o_output, ParsedArgs),
    Type     = list_to_atom(pkgargs:get(o_type, ParsedArgs)),
    Files    = perform_wildcard_matches(pkgargs:get(default,  ParsedArgs)),
    ConcMode = case pkgargs:get(o_one_thread, ParsedArgs) of
                   true  -> single_threaded;
                   false -> multi_threaded
               end,

    try branch(ShowHelp, ShowVsn, OutDir, Type, Files, ConcMode) of
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
-spec branch(boolean(),
             boolean(),
             file:filename_all(),
             pipeline_stage(),
             [file:filename_all(), ...],
             concurrency_mode()) -> ok | no_return().
branch(_Help = false, _Vsn = false, Outdir, all, Files, ConcMode) when (length(Files) > 0) ->
    transpile(Files, Outdir, core, ConcMode),
    transpile(Files, Outdir, core_ast, ConcMode),
    transpile(Files, Outdir, js_ast, ConcMode),
    transpile(Files, Outdir, js, ConcMode);
branch(_Help = false, _Vsn = false, Outdir, Type, Files, ConcMode) when (length(Files) > 0) ->
    transpile(Files, Outdir, Type, ConcMode);
branch(_Help = true, _Vsn = false, _OutDir, _Type, _Files = [], _ConcMode) ->
    throw(help);
branch(_Help = false, _Vsn = true, _OutDir, _Type, _Files = [], _ConcMode) ->
    throw(vsn);
branch(_Help, _Vsn, _OutDir, _Type, _Files, _ConcMode) ->
    throw(usage).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - COMPILER PIPELINE -------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%


-spec transpile([file:filename_all()],
                file:filename_all(),
                pipeline_stage(),
                concurrency_mode()) -> ok.
%% Convinience function for synchronously transpiling erl files in js files.
%% Also sets up compilation environment by extracting files to a tmp directory. Also cleans this
%% once transpilation is complete.
transpile(Files, Outdir, js, single_threaded) ->
    Codegen = pkgutils:pkg_extract_file("codegen.js"),
              pkgutils:pkg_extract_dir("node_modules.zip"),
    ASTs = [pipeline(js_ast, File, single_threaded) || File <- Files],
    [gen_js(Data, Module, Codegen, Outdir) || {Module, Data} <- ASTs],
    pkgutils:pkg_clean_tmp_dir(),
    ok;

%% Convinience function for asynchronously transpiling erl files into js files, and synchronously
%% returning after all processes finish.
%% Also sets up compilation environment by extracting files to a tmp directory. Once transpilation
%% finishes, it then cleans up said directory
transpile(Files, Outdir, js, multi_threaded) ->
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
    pkgutils:pkg_clean_tmp_dir(),
    ok;

%% Convinience function to synchronously transpile erl files into core_ast files
transpile(Files, Outdir, Type, single_threaded) ->
    ok = is_valid_type(Type),
    Processed = [pipeline(Type, File, single_threaded) || File <- Files],
    [write_other(io_lib:format("~p", [Data]), Module, Type, Outdir) || {Module, Data} <- Processed],
    ok;

%% Convinience function for asynchronously transpiling erl files into core_ast files, and synchronously
%% returning after all processes finish.
transpile(Files, Outdir, Type, multi_threaded) ->
    ok = is_valid_type(Type),
    Pids = [spawn_transpilation_process(File, Type, self()) || File <- Files],

    % Write results to file
    [
        receive {Pid, {Module, Data}} ->
            write_other(io_lib:format("~p", [Data]), Module, Type, Outdir)
        end
        || Pid <- Pids
    ],
    ok.

%% Spawns a process which performs transpilation for any given file.
%% Returns output to the spawning process
-spec spawn_transpilation_process(file:filename_all(),
                                  pipeline_stage(),
                                  pid()) -> pid().
spawn_transpilation_process(File, Type, MainProcess) ->
    spawn_link(fun() ->
        MainProcess ! {self(), pipeline(Type, File, multi_threaded)}
    end).

%% Shorthand for calling pipeline/3 where the function called is pipeline going as far as
%% the pipeline is currently implemented, assuming concurrency mode is 'single_threaded'
-spec pipeline(file:filename_all()) -> {file:filename_all(), any()}.
pipeline(Module) ->
    pipeline(js_ast, Module, single_threaded).

%% Shorthand for calling pipeline/3 where the function called is pipeline going as far as
%% the pipeline is currently implemented, with the specified concurrency mode.
-spec pipeline(file:filename_all(),
               concurrency_mode()) -> {file:filename_all(), any()}.
pipeline(Module, ConcMode) ->
    pipeline(js_ast, Module, ConcMode).

%% Calls different modules, all of which implement different parts of our compiler pipeline,
%% and returns the result of that stage of the pipeline to the calling function.
%% The first parameter should be an atom representing what stage of the pipeline you would like
%% to run and produce, such as 'core' or 'core_ast' to generate core_erlang or
%% a core_erlang_ast respectively.
-spec pipeline(pipeline_stage(),
               file:filename_all(),
               concurrency_mode()) -> {file:filename_all(), any()}.
pipeline(core, Module, _ConcMode) ->
    {ok, _ModuleName, BinaryData} = coregen:to_core_erlang(Module, return),
    {Module, BinaryData};
pipeline(core_ast, Module, _ConcMode) ->
    {ok, AST} = coregen:to_core_erlang_ast(Module, return),
    {Module, AST};
pipeline(js_ast, Module, ConcMode) ->
    {_Module, AST} = pipeline(core_ast, Module, ConcMode),
    {Module, estree:to_list(asttrans:erast_to_esast(AST, ConcMode))}.

%% Generate javascript by writing out a javascript AST to a file and passing it into codegen.js
%% Then proceeds to read the output of codegen.js and writes it to a file
-spec gen_js(estree:es_ast(),
             file:filename_all(),
             file:filename_all(),
             file:filename_all()) -> ok | no_return().
gen_js(AST, File, Codegen, Outdir) ->
    EncodedAST = jsone:encode(AST),

    %% We need to write our json into a temp file so that we can easily pass it into codegen.js
    %% We'll dump the temp file into the same directory as codegen.js
    WorkingDirectory = filename:dirname(Codegen),
    Filename = filename:basename(filename:rootname(File)),
    TempFile = lists:flatten([WorkingDirectory, "/", Filename, ".json"]),
    filepath:write(EncodedAST, TempFile),

    %% Pass json file into escodegen to generate JavaScript
    try os:cmd("node " ++ Codegen ++ " " ++ TempFile) of
        Result ->
            write_js(Result, Filename, Outdir)
    catch
        E ->
            {err, E}
    end.

%% Write results of codegen.js to a file
-spec write_js(any(),
               file:filename_all(),
               file:filename_all()) -> ok | no_return().
write_js(Result, Filename, Outdir) ->
    write_file(Result, Filename, "js", Outdir).

-spec write_other(any(),
                  file:filename_all(),
                  pipeline_stage(),
                  file:filename_all()) -> ok | no_return().
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
-spec write_file(any(),
                 file:filename_all(),
                 nonempty_string(),
                 file:filename_all()) -> ok;
                (any(),
                 file:filename_all(),
                 nonempty_string(), io) -> no_return().
write_file(Result, Filename, Ext, "io") ->
    write_file(Result, Filename, Ext, io); % dirty hack for dialyzer
write_file(Result, Filename, _Ext, io) ->
    io:format("==> Result of transpilation for: ~s.erl~n~s~n", [Filename, Result]);
write_file(Result, Filename, Ext, Outdir) ->
    filelib:ensure_dir(Outdir),
    ok = file:write_file(lists:flatten([Outdir, "/", Filename, ".", Ext]), Result).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - MISC FUNCTIONS ----------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Displays usage information
-spec usage() -> no_return().
usage() ->
    SelfName = pkgutils:pkg_name(),
    io:format("Usage: ~s FILEs... [-o <dirname>][-t <type>]~n" ++
              "Compiles the Erlang source files FILEs you specify into JavaScript.~n" ++
              "Or if Type is set will output a file from that point in the pipeline.~n" ++
              "Example: ~s src/*.erl -o js/~n~n",
              [SelfName,
               SelfName]).

%% Displays help information
-spec help() -> no_return().
help() ->
    io:format("Valid Configuration Parameters:~n" ++
              "~s~n",
              [pkgargs:create_help_string(?DEFAULT_ARGS, 1, 55)]).

%% Displays version information
-spec version() -> no_return().
version() ->
    io:format("Current ~s version: v~s~n~n",
              [pkgutils:pkg_name(),
              ?VERSION]).

%% Looks through a list of file names and expands any wildcards that may exist.
%% None wildcards will just be added to the accumulator so that we can crash gracefully
%% later on.
-spec perform_wildcard_matches([file:filename_all()]) -> [file:filename_all()].
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
-spec is_valid_type(pipeline_stage()) -> ok.
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