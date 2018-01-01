-module(consTest).
-compile(export_all).

func()->
    [].
func(A)->
    [A].
func(A,B)->
    [A,B].
func(A,B,C)->
    [A,B,C].

nest()->
    [[]].
nest(A)->
    [[A]].
nest(A,B)->
    [[A],B].
nest(A,B,C)->
    [[A,[B]],C].

test([a|Rest])->
    Rest;
test([a,B|Rest])->
    B.
