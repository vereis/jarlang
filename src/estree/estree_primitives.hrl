% Generates an Identifier Node
identifier(Name) when ?IS_IDENTIFIER(Name) ->
    node("Identifier", [{"name", Name}]).

identifier_test() ->
    ?assertEqual(identifier(<<"test_identifier">>), #{"type" => <<"Identifier">>, "name" => <<"test_identifier">>}).

is_identifier(?NODETYPE(<<"Identifier">>)) ->
    true;
is_identifier(_) ->
    false.

% Generates a SourceLocation node
sourceLocation(LineNumber, ColStart, ColEnd) ->
    ?spec([{LineNumber, integer}, {ColStart, integer}, {ColEnd, integer}]),
    node("SourceLocation", [{"source", null}, {"start", position(LineNumber, ColStart)}, {"end", position(LineNumber, ColEnd)}]).

sourceLocation_test() ->
    ?assertEqual(sourceLocation(10, 15, 12), #{
        "type"   => <<"SourceLocation">>, 
        "source" => null,
        "start"  => #{
            "type"   => <<"Position">>,
            "line"   => 10,
            "column" => 15
        },
        "end"    => #{
            "type"   => <<"Position">>,
            "line"   => 10,
            "column" => 12
        }
    }).

is_sourceLocation(?NODETYPE(<<"SourceLocation">>)) ->
    true;
is_sourceLocation(_) ->
    false.

position(Line, Col) ->
    ?spec([{Line, integer}, {Col, integer}]),    
    node("Position", [{"line", Line}, {"column", Col}]).

position_test() ->
    ?assertEqual(position(10, 15), #{
        "type"   => <<"Position">>,
        "line"   => 10,
        "column" => 15
    }).

is_position(?NODETYPE(<<"Position">>)) ->
    true;
is_position(_) ->
    false.

% Generates a Literal Node
literal(Value) when ?IS_LITERAL(Value) ->
    node("Literal", [{"value", Value}]).

literal_test() ->
    ?assertEqual(literal("abcde"), #{
        "type" => <<"Literal">>,
        "value" => "abcde"
    }),
    ?assertEqual(literal(12), #{
        "type" => <<"Literal">>,
        "value" => 12
    }),
    ?assertEqual(literal(-12), #{
        "type" => <<"Literal">>,
        "value" => -12
    }),
    ?assertEqual(literal(<<"proper string">>), #{
        "type" => <<"Literal">>,
        "value" => <<"proper string">>
    }),
    ?assertEqual(literal(true), #{
        "type" => <<"Literal">>,
        "value" => true
    }),
    ?assertEqual(literal(null), #{
        "type" => <<"Literal">>,
        "value" => null
    }),
    ?assertEqual(literal(12.6432363262), #{
        "type" => <<"Literal">>,
        "value" => 12.6432363262
    }).

is_literal(?NODETYPE(<<"Literal">>)) ->
    true;
is_literal(_) ->
    false.

% Generates a RegExp Literal
regex(Pattern, Flags) ->
    updateRecord(literal(null), [{"regex", #{pattern => Pattern, flags => Flags}}]).

regex_test() ->
    ?assertEqual(regex(1234, g), #{
        "regex" => #{flags => g,pattern => 1234},
        "type" => <<"Literal">>,
        "value" => null
    }).

is_regex(#{"regex" := _}) ->
    true;
is_regex(_) ->
    false.

% Generates a Program Node
program(Statements) ->
    ?spec([{Statements, list_of_statement}]),
    node("Program", [{"body", Statements}]).

program_test() ->
    ?assertEqual(program([emptyStatement()]), #{
        "body" => [
            #{
                "type" => <<"EmptyStatement">>
            }
        ],
        "type" => <<"Program">>
    }).

is_program(?NODETYPE(<<"Program">>)) ->
    true;
is_program(_) ->
    false.

% Generates a declaration
declaration() ->
    node().

declaration_test() ->
    ?assertEqual(is_declaration(declaration()), true).

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

expression_test() ->
    ?assertEqual(is_expression(expression()), true).

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

statement_test() ->
    ?assertEqual(is_statement(statement()), true).

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

spreadElement_test() ->
    ?assertEqual(spreadElement(identifier("whatever")), #{
        "argument" => #{
            "name" => "whatever",
            "type" => <<"Identifier">>
        },
        "type" => <<"SpreadElement">>
    }).

is_spreadElement(#{"type" := <<"SpreadElement">>}) ->
    true;
is_spreadElement(_) ->
    false.