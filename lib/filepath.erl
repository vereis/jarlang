% Author: Chris Bailey
% File path utility
% Extracts data from filepaths passed in
% Can also manipulate files in filesystem
-module(filepath).
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

% Shortcut functions
name(FilePath) ->
    {{_, Result}, _, _} = parse(FilePath),
    Result.

nameext(FilePath) ->
    {{_, Name}, _, {_, Ext}} = parse(FilePath),
    Name ++ Ext.

path(FilePath) ->
    {_, {_, Result}, _} = parse(FilePath),
    Result.

all(FilePath) ->
    {{_, Name}, {_, Path}, {_, Ext}} = parse(FilePath),
    Path ++ Name ++ Ext.

extension(FilePath) ->
    {_, _, {_, Result}} = parse(FilePath),
    Result.

% Move a file to a specific directory, if directory doesn't exist, create it.
move(Origin, Target) ->
    assertDir(path(Target)),
    file:rename(Origin, Target).

delete(Target) ->
    file:delete(Target).    

write(Data, Target) ->
    assertDir(path(Target)),
    file:write_file(Target, Data).

assertDir(Path) ->
    % We generally don't care what happens here, both cases ensure dir exists
    try file:make_dir(Path) of
        _ -> ok
    catch
        _ -> ok
    end.

% Parses a filepath and extracts information from it
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

% Utility functions
binlist_to_strlist(BinList) ->
    [binary_to_list(X) || X <- BinList].
