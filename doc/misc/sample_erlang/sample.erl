-module(sample).
-export([test/0,emptyFunction/0]).

test() ->
	print("############################################"),
	print("Start of tests."),
	testAssignmet(),
	testArithmetic(),
	testBigInt(),
	testAtoms(),
	testBooleanAlgebra(),
	testComparisons().


testAssignmet()->
	print("Test the assignment of values."),
	print(_ = 42),
	print(_ = $A),
	print(_ = $\n),
	print(_ = 2#101),
	print(_ = 16#1f),
	formatAndPrint(_ = 2.3),
	formatAndPrint(_ = 2.3e-3),
	print(47 = 45 + 2).

testArithmetic()->
	print("Test that arithmetic is correct."),
	print(17==2+15),
	print(4900==49*100),
	print(420==1892-1472),
	print(2.5==5/2),
	print(2==5 div 2),
	print(1==5 rem 2),
	print(1==(50 * 100) - 4999),
	print(-1==-(50 * 100 - 4999)),
	print(244950==-50 * (100 - 4999)),
	print("Test missing spaces in arithmetic."),
	print(2==5div 2),
	print(1==5rem 2).

testBigInt()->
	print("Test that big Ints are handled correctly"),
	X = 9007199254740992,
	Y = -X,
	print(not(X==X+1)),
	print(not(Y==Y-1)).

testAtoms()->
	print("Test atom definition."),
	print(atom),
	print(atoms_with_underscores),
	print(atom@with@at),
	A = 'Atom in single quotes!',
	print(A),
	A2 = '*Atom in single quotes!*',
	print(A2),
	print(atom='atom').

testBooleanAlgebra()->
	print("Test boolean algebra."),
	print(true),
	print(not false),
	print(false or true),
	print(true xor false),
	print(not true xor true),
	print(not (true xor true)),
	print(not (true and false)),
	print(not (false andalso printIfEvaluated(false))),
	print(true orelse printIfEvaluated(false)).

printIfEvaluated(Value)->
	print("right-side evaluated erroneously."),
	Value.

testComparisons()->
	print("Testing numeric comparisons."),
	print(5=:=5),
	print(not (1=:=0)),
	print(1=/=0),
	print(not (5=:=5.0)),
	print(5==5.0),
	print(not (5/=5.0)),
	print(1<2),
	print(not(1<1)),
	print(1>=1),
	print(1=<2),
	print("CrossType comparisons."),
	print(not(5=:=true)),
	print(not(0==false)),
	print(1<atom),
	print(1<true),
	print(1<false),
	%print( reference comparison here?
	print(atom<emptyFunction),
	PID = spawn(?MODULE, emptyFunction, []),
	print(emptyFunction<PID),
	print(PID<{1,2}),
	print({3,4}<[5,6]),
	print([7,8]< <<9,10>>).

emptyFunction()->
	true.









print(Input)->
	if
		is_list(Input)->
			io:format("~s~n",[Input]);
		is_atom(Input)->
			io:format("~s~n",[Input]);
		is_boolean(Input)->
			io:format("~s~n",[Input]);
		is_integer(Input)->
			io:format("~s~n",[integer_to_list(Input)]);
		is_float(Input)->
			io:format("~s~n",[float_to_list(Input)])
	end.

formatAndPrint(Number)->
	io:format(io_lib:format("~p~n",[Number])).