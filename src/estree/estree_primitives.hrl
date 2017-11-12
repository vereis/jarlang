% Generates an Identifier Node
identifier(Name) when ?IS_IDENTIFIER(Name) ->
    node("Identifier", [{"name", Name}]).

is_identifier(?NODETYPE(<<"Identifier">>)) ->
    true;
is_identifier(_) ->
    false.

% Generates a SourceLocation node
sourceLocation(LineNumber, ColStart, ColEnd) ->
    ?spec([{LineNumber, integer}, {ColStart, integer}, {ColEnd, integer}]),
    node("SourceLocation", [{"source", null}, {"start", position(LineNumber, ColStart)}, {"end", position(LineNumber, ColEnd)}]).

is_sourceLocation(?NODETYPE(<<"SourceLocation">>)) ->
    true;
is_sourceLocation(_) ->
    false.

position(Line, Col) ->
    ?spec([{Line, integer}, {Col, integer}]),    
    node("Position", [{"line", Line}, {"column", Col}]).

is_position(?NODETYPE(<<"Position">>)) ->
    true;
is_position(_) ->
    false.

% Generates a Literal Node
literal(Value) when ?IS_LITERAL(Value) ->
    node("Literal", [{"value", Value}]).

is_literal(?NODETYPE(<<"Literal">>)) ->
    true;
is_literal(_) ->
    false.

% Generates a RegExp Literal
regex(Pattern, Flags) ->
    updateRecord(literal(null), [{"regex", #{pattern => Pattern, flags => Flags}}]).

is_regex(#{"regex" := _}) ->
    true;
is_regex(_) ->
    false.

% Generates a Program Node
program(Statements) ->
    ?spec([{Statements, list_of_statement}]),
    node("Program", [{"body", Statements}]).

is_program(?NODETYPE(<<"Program">>)) ->
    true;
is_program(_) ->
    false.

% Generates a declaration
declaration() ->
    node().

is_declaration(#{"type" := Type}) ->
    case re:run(Type, "Declaration") of
        {match, _} ->
            true;
        _ ->
            false
    end;
is_declaration(#{}) ->
    true;
is_declaration(_) ->
    false.

% Generate an Expression
expression() ->
    node().

is_expression(#{"type" := Type}) ->
    case re:run(Type, "SpreadElement|Expression|Literal|Identifier") of
        {match, _} ->
            true;
        _ ->
            false
    end;
is_expression(#{}) ->
    true;
is_expression(_) ->
    false.

% Generates a generic Statement Node
statement() ->
    node().

is_statement(#{"type" := Type}) ->
    case re:run(Type, "Statement|Declaration") of
        {match, _} ->
            true;
        _ ->
            false
    end;
is_statement(#{}) ->
    true;
is_statement(_) ->
    false.

% SpreadExpression 
spreadElement(Argument) ->
    ?spec([{Argument, [identifier, arrayExpression]}]),
    updateRecord(node(), [{"type", <<"SpreadElement">>}, {"argument", Argument}]).

is_spreadElement(#{"type" := <<"SpreadElement">>}) ->
    true;
is_spreadElement(_) ->
    false.