-module(listCompTest).
-compile(export_all).

func(List)->
    [ E || E <- List, is_number(E) ].
