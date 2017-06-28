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
tokenize({Literal, Text, LineNo, ColNo}) ->
	[{Literal, {text, Text}, {line_no, LineNo}, {col_no, ColNo}}].  

scan(Input) ->
	scan(Input, [], []).

scan([], Lexemes) ->
	Lexemes.

scan([], Lexemes, CharBuffer) ->
	Text = CharBuffer,
	scan([], Lexemes ++ tokenize({eof, Text, null, null}));

scan([Head = <<"(">> | Tail], Lexemes, CharBuffer) ->
	Text = CharBuffer ++ [Head],
	scan(Tail, Lexemes ++ tokenize({paren_l, Text, null, null}), []);

scan([Head = <<")">> | Tail], Lexemes, CharBuffer) ->
	Text = CharBuffer ++ [Head],
	scan(Tail, Lexemes ++ tokenize({paren_r, Text, null, null}), []);

scan([Head|Tail], Lexemes, CharBuffer) ->
	Text = CharBuffer ++ [Head],
	scan(Tail, Lexemes, Text).




% Lexical Error Reporting
err({Emsg, Eline}) ->
	io:format("Error: ~s~nOn line ~s ~n", [Emsg, Eline]).
