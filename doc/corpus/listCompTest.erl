-module(listCompTest).
-compile(export_all).

func(List)->
    [ E || E <- List, is_number(E) ].

% filter(Pred, List) when is_function(Pred, 1) ->
%    [ E || E <- List, Pred(E) ].