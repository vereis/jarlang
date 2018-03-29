-module(caseTest).
-compile(export_all).


func(A)->
    case A of
        foo -> bar;
        bar -> foo;
        B when B>5 -> three;
        B -> four
    end.


keysort_1(I, X, EX, [Y | L], R) ->
    case element(I, Y) of
	EY when EX =< EY ->
	    one;
	EY ->
	    two
    end.
