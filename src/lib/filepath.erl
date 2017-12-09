%%% Library to extract components from filepaths and useful
%%% shortcuts into the file module to allow manipulation of files 
%%% and filepaths
-module(filepath).
-author(["Chris Bailey"]).
-vsn(1.0).

-export([
    parse/1,
    name/1,
    nameext/1,
    path/1,
    all/1,
    extension/1,
    move/2,
    write/2,
    delete/1
]).

%% Gets a filename segment of a filepath, excluding file extension
name(FilePath) ->
    {{_, Result}, _, _} = parse(FilePath),
    Result.

%% Gets the name and extension of the filename segment of a filepath
nameext(FilePath) ->
    {{_, Name}, _, {_, Ext}} = parse(FilePath),
    Name ++ Ext.

%% Gets the path segment of a filepath, excluding filename
path(FilePath) ->
    {_, {_, Result}, _} = parse(FilePath),
    Result.

%% Gets a string representing the path/.../filename.extension of a filepath
all(FilePath) ->
    {{_, Name}, {_, Path}, {_, Ext}} = parse(FilePath),
    Path ++ Name ++ Ext.

%% Gets the extension of the filename segment of a filepath
extension(FilePath) ->
    {_, _, {_, Result}} = parse(FilePath),
    Result.

%% Move a file to a specific directory, if directory doesn't exist, create it
move(Origin, Target) ->
    assertDir(path(Target)),
    file:rename(Origin, Target).

%% Delete a file 
delete(Target) ->
    file:delete(Target).    

%% Write data to a file, if a directory is given which doesnt exist, create it
write(Data, Target) ->
    assertDir(path(Target)),
    file:write_file(Target, Data).

%% Checks to see if a filepath exists, and if it doesnt, creates it.
assertDir(Path) ->
    try file:make_dir(Path) of
        _ -> ok
    catch
        _ -> ok
    end.

%% Parses a filepath and extracts information from it
parse(FilePath) ->
    parse(re:split(FilePath, "/"), []).

parse([<<>>], []) ->
    {error, nothing_to_parse};
parse([], [F | P]) ->
    PathList   = binlist_to_strlist(lists:reverse(P)),
    case string:join(PathList, "/") of
        [] ->
            Path = [];
        Result ->
            Path = Result ++ "/"
    end,

    [Name | Extensions] = binlist_to_strlist(re:split(F, "\\.")),  

    {
        {filename, Name}, 
        {filepath, Path}, 
        {extension, string:join(Extensions, ".")}
    };
parse([Head | Tails], PathList) ->
    parse(Tails, [Head] ++ PathList).

%% Utility functions
binlist_to_strlist(BinList) ->
    [binary_to_list(X) || X <- BinList].
