-define(IS_LITERAL(X),
        is_list(X) ; is_binary(X) ; X =:= null ; X =:= undefined ; is_integer(X) ; is_float(X) ; is_boolean(X)).
-define(IS_IDENTIFIER(X),
	is_list(X) ; is_binary(X)).

% Generates an Identifier Node
identifier(Name) when ?IS_IDENTIFIER(Name) ->
    node("Identifier", [{"name", Name}]).

% Generates a Literal Node
literal(Value) when ?IS_LITERAL(Value) ->
    node("Literal", [{"value", Value}]).

% Generates a RegExp Literal
regexLiteral(Pattern, Flags) ->
    updateRecord(literal(null), [{regex, #{pattern => Pattern, flags => Flags}}]).

% Generates a Program Node
program(Statements) when is_list(Statements) ->
    node("Program", [{"body", Statements}]);
program(Statements) ->
    program([Statements]).

% Generates a declaration
declaration() ->
    node().

% Generate an Expression
expression() ->
    node().

% Generates a generic Statement Node
statement() ->
    node().