-module(consTest).
-compile(export_all).


func(A,C={foo,B},D={E,bar})->
    [A,B,C,D].

