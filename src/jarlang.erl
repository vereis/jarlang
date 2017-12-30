%%% Module Description:
%%% Main entrypoint into compiler
-module(jarlang).
-author(["Chris Bailey", "Andrew Johnson"]).

-define(VERSION, "1.1.0").

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
    Files    = perform_wildcard_matches(pkgargs:get(default,  ParsedArgs)),

    try branch(ShowHelp, ShowVsn, OutDir, Files) of
        _ -> ok
    catch
        throw:usage ->
            usage(),
            help();
        throw:help ->
            help();
        throw:vsn ->
            version()
    end,

    % Clean up and stop
    init:stop().





%%% ---------------------------------------------------------------------------------------------%%%
%%% - ENTRYPOINT CODE ---------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Determines which function to run depending on arguments given.
branch(_Help = false, _Vsn = false, Outdir, Files) when length(Files) > 0 ->
    transpile(Files, Outdir);
branch(_Help = true, _Vsn = false, _OutDir, _Files = []) ->
    throw(help);
branch(_Help = false, _Vsn = true, _OutDir, _Files = []) ->
    throw(vsn);
branch(_Help, _Vsn, _OutDir, _Files) ->
    throw(usage).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - COMPILER PIPELINE -------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Convinience function for asynchronously transpiling erl files in js files, and synchronously
%% returning after all processes finish.
%% Also sets up compilation environment by extracting files to a tmp directory. Once transpilation
%% finishes, it then cleans up said directory
transpile(Files, Outdir) ->
    Codegen = pkgutils:pkg_extract_file("codegen.js"),
              pkgutils:pkg_extract_dir("node_modules.zip"),
    Pids = [spawn_transpilation_process(File, self()) || File <- Files],

    %% Pass transpilated results into codegen.js to get JavaScript which we can write to a file
    [
        receive {Pid, {Module, Ast}} ->
            gen_js(Ast, Module, Codegen, Outdir)
        end
        || Pid <- Pids
    ],
    pkgutils:pkg_clean_tmp_dir().

%% Spawns a process which performs transpilation for any given file.
%% Returns output to the spawning process
spawn_transpilation_process(File, MainProcess) ->
    spawn_link(fun() ->
        MainProcess ! {self(), pipeline(File)}
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
    BinaryData;
pipeline(core_ast, Module) ->
    {ok, AST} = coregen:to_core_erlang_ast(Module, return),
    AST;
pipeline(js_ast, Module) ->
    AST = pipeline(core_ast, Module),
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
write_js(Result, Filename, "io") ->
    io:format("==> Result of transpilation for: ~s.erl~n~s~n", [Filename, Result]);
write_js(Result, Filename, Outdir) ->
    filelib:ensure_dir(Outdir),
    ok = file:write_file(lists:flatten([Outdir, "/", Filename, ".js"]), Result).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - MISC FUNCTIONS ----------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Displays usage information
usage() ->
    SelfName = pkgutils:pkg_name(),
    io:format("Usage: ~s FILEs... [-o <dirname>]~n" ++
              "Compiles the Erlang source files FILEs you specify into JavaScript.~n" ++
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