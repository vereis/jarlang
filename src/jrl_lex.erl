% lex.erl
% Lexer implementation for Jrlscript Compiler
-module(jrl_lex).
-compile(export_all).

lex() ->
	io:format("Usage - lex(Input<String>).~n", []). 

lex(Input) ->
	Lexemes = re:split(Input, "", [{return, binary}]),
	scan(Lexemes).	

scan(Lexemes) ->
	Lexemes. 

err({Emsg, Eline}) ->
	io:format("Error: ~s~nOn line ~s ~n", [Emsg, Eline]).	
