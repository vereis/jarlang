% Author: Nick Laine
% Provides a means of extracting token data from a given file.

-module(tokdata).
-export([get_exported_function_map/1]).


% Returns a map of the line numbers of exported functions keyed by function name/arity.
% Throughout the file, this key is referred to as an "export atom" or "definition atom".
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


% Returns a list of atoms representing exported functions, i.e. the keys in the map of export data.
process_exports(Toks) when is_list(Toks) ->
    process_exports(Toks, []);

process_exports(_) ->
    badarg.

process_exports([{atom, _, Name} | [{'/', _} | [{integer, _, Arity} | Toks]]], Exp) ->
    process_exports(Toks, [list_to_atom(atom_to_list(Name) ++ "/" ++ integer_to_list(Arity)) | Exp]);

process_exports([{',', _} | Toks], Exp) ->
    process_exports(Toks, Exp);

process_exports([{']', _} | [{')', _} | _Toks]], Exp) ->
    lists:reverse(Exp);

process_exports(_, _) ->
    badexp.


% Searches file tokens for the definitions of exported functions.
% Maps their line numbers to their corresponding export atoms.
match_export_lines([{atom, Line, Name} | [{'(', _} | Toks]], ExpList, ExpMap) ->
    {NextToks, DefAtom} = process_export_atom(Toks, Name, 0),
    {NewList, Found} = match_export_atom(DefAtom, ExpList, []),
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


% Processes tokens that are believed to belong to a function definition, and builds a definition atom accordingly.
% todo: Make more thorough, as it can't distinguish a function call from a definition until the closing arg bracket.
process_export_atom([{',', _} | Toks], Name, Arity) ->
    process_export_atom(Toks, Name, Arity + 1);

process_export_atom([{')', _} | [{'->', _} | Toks]], Name, Arity) ->
    {Toks, list_to_atom(atom_to_list(Name) ++ "/" ++ integer_to_list(Arity))};

process_export_atom([{')', _} | Toks], _Name, _Arity) ->
    {Toks, notdef};

process_export_atom([_ | Toks], Name, 0) ->
    process_export_atom(Toks, Name, 1);

process_export_atom([_ | Toks], Name, Arity) ->
    process_export_atom(Toks, Name, Arity).


% Checks whether a definition atom matches an exported function, and removes it from the list of export atoms if found.
match_export_atom(notdef, Exp, []) ->
    {Exp, false};

match_export_atom(DefAtom, [DefAtom | Exp], Checked) ->
    {Checked ++ Exp, true};

match_export_atom(DefAtom, [E | Exp], Checked) ->
    match_export_atom(DefAtom, Exp, [E | Checked]);

match_export_atom(_DefAtom, [], Checked) ->
    {Checked, false}.

