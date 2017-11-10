% Generates an Identifier Node
identifier(Name) when ?IS_IDENTIFIER(Name) ->
    node("Identifier", [{"name", Name}]).

is_identifier(?NODETYPE(<<"Identifier">>)) ->
    true;
is_identifier(_) ->
    false.

% Generates a SourceLocation node
sourceLocation(LineNumber, ColStart, ColEnd) ->
    case {is_integer(LineNumber), is_integer(ColStart), is_integer(ColEnd)} of 
        {true, true, true} ->    
            node("SourceLocation", [{"source", null}, {"start", position(LineNumber, ColStart)}, {"end", position(LineNumber, ColEnd)}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [integer, integer, integer], [typeof(LineNumber), typeof(ColStart), typeof(ColEnd)])
    end.

is_sourceLocation(?NODETYPE(<<"SourceLocation">>)) ->
    true;
is_sourceLocation(_) ->
    false.

position(Line, Col) ->
    case {is_integer(Line), is_integer(Col)} of
        {true, true} ->
            node("Position", [{"line", Line}, {"column", Col}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [integer, integer], [typeof(Line), typeof(Col)])
    end.

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
    case {is_list(Statements), lists:all(fun(X) -> X =:= true end, lists:map(fun(X) -> is_statement(X) end, Statements))} of
        {true, true} ->
            node("Program", [{"body", Statements}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"List[Statements]">>], [{typeof(Statements), lists:map(fun(X) -> nodetype(X) end, Statements)}])
    end.

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
    true.

% Generate an Expression
expression() ->
    node().

is_expression(#{"type" := Type}) ->
    case re:run(Type, "Expression|Literal|Identifier") of
        {match, _} ->
            true;
        _ ->
            false
    end;
is_expression(#{}) ->
    true.

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
    true.