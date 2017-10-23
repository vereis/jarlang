% Author: Nick Laine
% Provides a means of extracting token data from a given file.

-module(tokdata).
-export([map_export_data/1]).

map_export_data(Filename) ->
    Toks = tokenize_file(Filename),
    match_export_lines(Toks, find_exports(Toks), #{}).


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


find_exports({ok, Toks, _}) ->
    find_exports(Toks);

find_exports([{atom, _LineNo, export} | Toks]) ->
    process_exports(Toks);

find_exports([_ | Toks]) ->
    find_exports(Toks);

find_exports([]) ->
    ok;

find_exports(error) ->
    error.


process_exports(Toks) when is_list(Toks) ->
    process_exports(Toks, []);

process_exports(_) ->
    badarg.

process_exports([{'(', _} | [{'[', _} | Toks]], Exp) ->
    process_exports(Toks, Exp);

process_exports([{',', _} | Toks], Exp) ->
    process_exports(Toks, Exp);

process_exports([{atom, _, Name} | [{'/', _} | [{integer, _, Arity} | Toks]]], Exp) ->
    process_exports(Toks, [list_to_atom(atom_to_list(Name) ++ "/" ++ integer_to_list(Arity)) | Exp]);

process_exports([{']', _} | [{')', _} | _Toks]], Exp) ->
    lists:reverse(Exp);

process_exports(_, _) ->
    badexp.


match_export_lines([{atom, Line, Name} | [{'(', _} | Toks]], ExpList, ExpMap) ->
    {NextToks, DefAtom} = get_definition_atom(Toks, Name, 0),
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


get_definition_atom([{',', _} | Toks], Name, Arity) ->
    get_definition_atom(Toks, Name, Arity + 1);

get_definition_atom([{')', _} | [{'->', _} | Toks]], Name, Arity) ->
    {Toks, list_to_atom(atom_to_list(Name) ++ "/" ++ integer_to_list(Arity))};

get_definition_atom([{')', _} | Toks], _Name, _Arity) ->
    {Toks, notdef};

get_definition_atom([_ | Toks], Name, 0) ->
    get_definition_atom(Toks, Name, 1);

get_definition_atom([_ | Toks], Name, Arity) ->
    get_definition_atom(Toks, Name, Arity).


match_export_atom(notdef, Exp, []) ->
    {Exp, false};

match_export_atom(DefAtom, [DefAtom | Exp], Checked) ->
    {Checked ++ Exp, true};

match_export_atom(DefAtom, [E | Exp], Checked) ->
    match_export_atom(DefAtom, Exp, [E | Checked]);

match_export_atom(_DefAtom, [], Checked) ->
    {Checked, false}.

