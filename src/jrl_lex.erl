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

scan([_Head | Tail], Lexemes, CharBuffer) ->
	err({"Unknown symbol", null}),
	scan(Tail, Lexemes, CharBuffer).




% Looks ahead until lambda passed in is truthy. Returns all characters until then.
lookahead(Input, Lambda) ->
	lookahead(Input, Lambda, [], []).

lookahead([], _WhilePredicate, Buffer, Remainder) ->
	{Buffer,  Remainder};
lookahead(List = [Head | Tail], WhilePredicate, Buffer, _Remainder) ->
	case WhilePredicate(Head) of
		true  -> lookahead([], WhilePredicate, Buffer, List);
		false -> lookahead(Tail, WhilePredicate, Buffer ++ [Head], [])
	end.




% Lexical Error Reporting
err({Emsg, Eline}) ->
	io:format("Error: ~s~nOn line ~s ~n", [Emsg, Eline]).
