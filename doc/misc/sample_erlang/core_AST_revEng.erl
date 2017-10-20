-module(core_AST_revEng).
-export([boolean/0,
	 integer/0,
	 string/0,
	 addition/1,
	 subtraction/1,
	 multiplication/1,
	 division/1,
	 sequence/0]).

boolean() -> true.
integer() -> 3.
string() ->"Hello, world!".

addition(Var) -> Var + 3.
subtraction(Var) -> Var - 3.

multiplication(Var) -> Var * 3.
division(Var) -> Var / 3.

sequence() ->
	io:format("A"),
	io:format("B"),
	io:format("C").


