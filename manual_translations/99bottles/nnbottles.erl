-module(nnbottles).
-export([sing/0, pluralise/2]).

sing() ->
    sing(99).

sing(N) when N < 0 ->
    done;
sing(N) ->
    verseone(N),
    versetwo(N-1),
    sing(N-1).

verseone(N) ->
    Bot = pluralise("bottle", N),
    io:format("~s ~s of beer on the wall, ~s ~s of beer~n", [count(N), Bot, count(N), Bot]).

versetwo(N) when N < 0 ->
    io:format("Go to the store and buy some more, 99 bottles of beer on the wall~n~n");
versetwo(N) ->
    io:format("Take one down and pass it around, ~s ~s of beer on the wall~n~n", [count(N), pluralise("bottle", count(N))]).

count(0) ->
    "No more";
count(N) ->
    integer_to_list(N).

pluralise(Word, 1) ->
    Word;
pluralise(Word, _N) ->
    Word ++ "s".
