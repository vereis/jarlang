% Generates an Identifier Node
identifier(Name) when is_binary(Name) ->
    node("Identifier", [{"name", Name}]).

% Generates a Literal Node
literal(Value) when is_binary(Value) ; Value =:= null ; is_integer(Value) ; is_float(Value) ; is_boolean(Value) -> % Need to add regex definition
    node("Literal", [{"value", Value}]).

% Generates a RegExp Literal
regexLiteral(Pattern, Flags) ->
    updateRecord(literal(null), [{regex, #{pattern => Pattern, flags => Flags}}]).

% Generates a Program Node
program(Statement) ->
    node("Program", [{"body", Statement}]).