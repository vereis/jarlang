% lex.erl
% Lexer implementation for Jrlscript Compiler
-module(jrl_lex).
-compile(export_all).

lex() ->
	io:format("Usage - lex(Input<String>).~n", []). 

lex(Input) ->
	SplitInput = re:split(Input, "", [{return, binary}]),
	scan(SplitInput).	




% Scanner Functions:
% Scans a list in the form [<<char>...] and generates Lexemes which will be returned 
scan(Input) ->
	[Head|Tail] = Input,
	scan(Tail, []).

scan([], Lexemes) ->
	Lexemes;

scan([<<"(">>|Tail], Lexemes) ->
	scan(Tail, Lexemes ++ [paren_l]);

scan([<<")">>|Tail], Lexemes) ->
	scan(Tail, Lexemes ++ [paren_r]);

scan([_|Tail], Lexemes) ->
	scan(Tail, Lexemes).




% Lexical Error Reporting
err({Emsg, Eline}) ->
	io:format("Error: ~s~nOn line ~s ~n", [Emsg, Eline]).	
