-module(core_AST_revEng).
-export([boolean/0,
     integer/0,
     string/0,
     addition/1,
     subtraction/1,
     multiplication/1,
     division/1,
     echo/1,
     sequence/0,
     errIfNot3/1,
     errIfNot3Tuple/1,
     errIfNot3and3/2]).

boolean() -> true.
integer() -> 3.
string() ->"Hello, world!".

addition(Var) -> Var + 3.
subtraction(Var) -> Var - 3.

multiplication(Var) -> Var * 3.
division(Var) -> Var / 3.

echo(Var)->Var.

sequence() ->
    io:format("A"),
    io:format("B"),
    io:format("C").

errIfNot3(Var) ->
    3 = Var.

errIfNot3Tuple(Var) ->
    {3,atom} = {Var,atom}.

errIfNot3and3(Var,Var2) ->
    {3,3} = {Var,Var2}.

caseExample(Var)->
    case Var of
        match ->
            42;
        othermatch ->
            not42;
        _ -> nomatch
    end.
