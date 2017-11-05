% Generates an Identifier Node
identifier(Name) when ?IS_IDENTIFIER(Name) ->
    node("Identifier", [{"name", Name}]).

is_identifier(?NODETYPE(<<"Identifier">>)) ->
    true;
is_identifier(_) ->
    false.

% Generates a SourceLocation node
sourceLocation(LineNumber, ColStart, ColEnd) ->
    node("SourceLocation", [{"source", null}, {"start", position(LineNumber, ColStart)}, {"end", position(LineNumber, ColEnd)}]).

is_sourceLocation(?NODETYPE(<<"SourceLocation">>)) ->
    true;
is_sourceLocation(_) ->
    false.

position(Line, Col) ->
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

is_regex(#{regex := _}) ->
    true;
is_regex(_) ->
    false.

% Generates a Program Node
program(Statements) when is_list(Statements) ->
    node("Program", [{"body", Statements}]);
program(Statements) ->
    program([Statements]).

is_program(?NODETYPE(<<"Program">>)) ->
    true;
is_program(_) ->
    false.

% Generates a declaration
declaration() ->
    node().

% Generate an Expression
expression() ->
    node().

% Generates a generic Statement Node
statement() ->
    node().