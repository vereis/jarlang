% Author: Nick Laine
% Provides a means of extracting token data from a given file.

-module(tokdata).
-export([get_exported_function_map/1]).


% Returns a map of the line numbers of exported functions keyed by function name/arity.
% Throughout the file, this key is referred to as an "export string" or "definition string".
get_exported_function_map(Filename) ->
    Toks = tokenize_file(Filename),
    match_export_lines(Toks, find_exports(Toks), #{}).


% Scans and tokenizes a given erlang file; not dissimilar to coregen code that tokenizes core erlang.
% todo: add tokenize_erl/tokenize_core functions to lib?
tokenize_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Bin} ->
            case erl_scan:string(binary_to_list(Bin)) of
                {ok, Toks, _} ->
                    Toks;
                {error, _, _} ->
                    error
            end;
        {error, _} ->
            error
    end.


% Search file tokens for an export directive, then perform processing.
find_exports({ok, Toks, _}) ->
    find_exports(Toks);

find_exports([{'-', _} | [{atom, _, export} | [{'(', _} | [{'[', _} | Toks]]]]) ->
    process_exports(Toks);

find_exports([_ | Toks]) ->
    find_exports(Toks);

find_exports([]) ->
    ok;

find_exports(error) ->
    error.


% Returns a list of strings representing exported functions, i.e. the keys in the map of export data.
process_exports(Toks) when is_list(Toks) ->
    process_exports(Toks, []);

process_exports(_) ->
    badarg.

process_exports([{atom, _, Name} | [{'/', _} | [{integer, _, Arity} | Toks]]], Exp) ->
    process_exports(Toks, [atom_to_list(Name) ++ "/" ++ integer_to_list(Arity) | Exp]);

process_exports([{',', _} | Toks], Exp) ->
    process_exports(Toks, Exp);

process_exports([{']', _} | [{')', _} | _Toks]], Exp) ->
    lists:reverse(Exp);

process_exports(_, _) ->
    badexp.


% Searches file tokens for the definitions of exported functions.
% Maps their line numbers to their corresponding export strings.
match_export_lines([{atom, Line, Name} | [{'(', _} | Toks]], ExpList, ExpMap) ->
    {NextToks, DefAtom} = process_export_string(Toks, Name, 0),
    {NewList, Found} = match_export_string(DefAtom, ExpList, []),
    case Found of
        true ->
            match_export_lines(NextToks, NewList, maps:put(DefAtom, Line, ExpMap));
        _ ->
            match_export_lines(NextToks, NewList, ExpMap)
    end;

match_export_lines([_ | Toks], ExpList, ExpMap) ->
    match_export_lines(Toks, ExpList, ExpMap);

match_export_lines(_Toks, [], ExpMap) ->
    ExpMap;

% Should be unreachable
match_export_lines([], ExpList, ExpMap) ->
    io:format("Unfound exports: ~p~n", [ExpList]),
    ExpMap.


% Processes tokens that are believed to belong to a function definition, and builds a definition string accordingly.
% todo: Make more thorough, as it can't distinguish a function call from a definition until the closing arg bracket.
process_export_string([{',', _} | Toks], Name, Arity) ->
    process_export_string(Toks, Name, Arity + 1);

process_export_string([{')', _} | [{'->', _} | Toks]], Name, Arity) ->
    {Toks, atom_to_list(Name) ++ "/" ++ integer_to_list(Arity)};

process_export_string([{')', _} | Toks], _Name, _Arity) ->
    {Toks, notdef};

process_export_string([_ | Toks], Name, 0) ->
    process_export_string(Toks, Name, 1);

process_export_string([_ | Toks], Name, Arity) ->
    process_export_string(Toks, Name, Arity).


% Checks whether a definition string matches an exported function, and removes it from the list of export strings if found.
match_export_string(notdef, Exp, []) ->
    {Exp, false};

match_export_string(DefAtom, [DefAtom | Exp], Checked) ->
    {Checked ++ Exp, true};

match_export_string(DefAtom, [E | Exp], Checked) ->
    match_export_string(DefAtom, Exp, [E | Checked]);

match_export_string(_DefAtom, [], Checked) ->
    {Checked, false}.

