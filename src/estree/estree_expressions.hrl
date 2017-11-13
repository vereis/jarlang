% Generate a This expression
thisExpression() ->
    updateRecord(expression(), [{"type", <<"ThisExpression">>}]).

thisExpression_test() ->
    ?assertEqual(thisExpression(), #{"type" => <<"ThisExpression">>}).

is_thisExpression(?NODETYPE(<<"ThisExpression">>)) ->
    true;
is_thisExpression(_) ->
    false.

% Generate an Array expression
arrayExpression(Elements) ->
    ?spec([{Elements, list_of_expression}]),
    updateRecord(expression(), [{"type", <<"ArrayExpression">>}, {"elements", Elements}]).

arrayExpression_test() ->
    ?assertEqual(arrayExpression([]), #{"type" => <<"ArrayExpression">>, "elements" => []}),
    ?assertEqual(arrayExpression([thisExpression()]), #{
        "type" => <<"ArrayExpression">>, 
        "elements" => [
            #{"type" => <<"ThisExpression">>}    
        ]
    }),
    ?assertEqual(arrayExpression([thisExpression(), thisExpression()]), #{
        "type" => <<"ArrayExpression">>, 
        "elements" => [
            #{"type" => <<"ThisExpression">>},
            #{"type" => <<"ThisExpression">>}    
        ]
    }).

is_arrayExpression(?NODETYPE(<<"ArrayExpression">>)) ->
    true;
is_arrayExpression(_) ->
    false.

% Generate an Object expression
objectExpression(Properties) ->
    ?spec([{Properties, list_of_property}]),
    updateRecord(expression(), [{"type", <<"ObjectExpression">>}, {"properties", Properties}]).

objectExpression_test() ->
    ?assertEqual(objectExpression([]), #{"type" => <<"ObjectExpression">>, "properties" => []}),
    ?assertEqual(objectExpression([property(literal("a"), identifier("b"))]), #{
        "type" => <<"ObjectExpression">>, 
        "properties" => [
            #{
                "key" => #{
                    "type" => <<"Literal">>,
                    "value" => "a"
                },
                "kind" => <<"init">>,
                "type" => <<"Property">>,
                "value" => #{
                    "name" => "b",
                    "type" => <<"Identifier">>
                }
            }    
        ]
    }).

is_objectExpression(?NODETYPE(<<"ObjectExpression">>)) ->
    true;
is_objectExpression(_) ->
    false.

% Generates a Function expression
functionExpression(Identifier, Params, Body, Expression) ->
    ?spec([{Identifier, [identifier, null]},
           {Params, list_of_identifier},
           {Body, blockStatement},
           {Expression, boolean}]),
    updateRecord(expression(), [{"type", <<"FunctionExpression">>}, {"id", Identifier}, {"params", Params}, {"body", Body}, {"expression", Expression}]).

functionExpression_test() ->
    ?assertEqual(functionExpression(identifier("a"), [], blockStatement([]), false), #{
        "body" => #{
            "body" => [],
            "type" => <<"BlockStatement">>
        },
        "expression" => false,
        "id" => #{
            "name" => "a",
            "type" => <<"Identifier">>
        },
        "params" => [],
        "type" => <<"FunctionExpression">>
    }).

is_functionExpression(?NODETYPE(<<"FunctionExpression">>)) ->
    true;
is_functionExpression(_) ->
    false.

% A Literal Property for Object expressions
property(Key, Value) ->
    ?spec([{Key, [literal, identifier]}, {Value, expression}]),
    node("Property", [{"key", Key}, {"value", Value}, {"kind", <<"init">>}]).

property_test() ->
    ?assertEqual(property(literal("a"), identifier("b")), #{
        "key" => #{
            "type" => <<"Literal">>,
            "value" => "a"
        },
        "kind" => <<"init">>,
        "type" => <<"Property">>,
        "value" => #{
            "name" => "b",
            "type" => <<"Identifier">>
        }
    }). 

is_property(?NODETYPE(<<"Property">>)) ->
    true;
is_property(_) ->
    false.

% Generate a sequence expression
sequenceExpression(Expressions) ->
    ?spec([{Expressions, list_of_expression}]),
    updateRecord(expression(), [{"type", <<"SequenceExpression">>}, {"expressions", Expressions}]).

sequenceExpression_test() ->
    ?assertEqual(sequenceExpression([thisExpression()]), #{
        "expressions" => [
            #{"type" => <<"ThisExpression">>}
        ],
        "type" => <<"SequenceExpression">>
    }).

is_sequenceExpression(?NODETYPE(<<"SequenceExpression">>)) ->
    true;
is_sequenceExpression(_) ->
    false.

% Generate a unary operation expression
unaryExpression(Operator, Prefix, Argument) when ?IS_UNARY_OPERATOR(Operator)  ->
    ?spec([{Operator, anything},
           {Prefix, boolean},
           {Argument, expression}]),
    updateRecord(expression(), [{"type", <<"UnaryExpression">>}, {"operator", Operator}, {"prefix", Prefix}, {"argument", Argument}]).

unaryExpression_test() ->
    ?assertEqual(unaryExpression(<<"-">>, true, literal(12)), #{
        "argument" => #{
            "type" => <<"Literal">>,
            "value" => 12
        },
        "operator" => <<"-">>,
        "prefix" => true,
        "type" => <<"UnaryExpression">>
    }),
    ?assertEqual(unaryExpression(<<"delete">>, true, literal(12)), #{
        "argument" => #{
            "type" => <<"Literal">>,
            "value" => 12
        },
        "operator" => <<"delete">>,
        "prefix" => true,
        "type" => <<"UnaryExpression">>
    }).

is_unaryExpression(?NODETYPE(<<"UnaryExpression">>)) ->
    true;
is_unaryExpression(_) ->
    false.

% Generate a binary operation expression
binaryExpression(Operator, Left, Right) when ?IS_BINARY_OPERATOR(Operator) ->
    ?spec([{Operator, anything},
           {Left, expression},
           {Right, expression}]),
    updateRecord(expression(), [{"type", <<"BinaryExpression">>}, {"operator", Operator}, {"left", Left}, {"right", Right}]).
        
binaryExpression_test() ->
    ?assertEqual(binaryExpression(<<"-">>, literal(10), literal(12)), #{
        "left" => #{"type" => <<"Literal">>,"value" => 10},
        "operator" => <<"-">>,
        "right" => #{"type" => <<"Literal">>,"value" => 12},
        "type" => <<"BinaryExpression">>
    }).

is_binaryExpression(?NODETYPE(<<"BinaryExpression">>)) ->
    true;
is_binaryExpression(_) ->
    false.

% Generate an update (increment or decrement) operator expression
updateExpression(Operator, Argument, Prefix) when ?IS_UPDATE_OPERATOR(Operator) ->
    ?spec([{Operator, anything},
           {Argument, expression},
           {Prefix, boolean}]),
    updateRecord(expression(), [{"type", <<"UpdateExpression">>}, {"operator", Operator}, {"argument", Argument}, {"prefix", Prefix}]).

updateExpression_test() ->
    ?assertEqual(updateExpression(<<"++">>, literal(20), true), #{
        "argument" => #{"type" => <<"Literal">>,"value" => 20},
        "operator" => <<"++">>,
        "prefix" => true,
        "type" => <<"UpdateExpression">>
    }).

is_updateExpression(?NODETYPE(<<"UpdateExpression">>)) ->
    true;
is_updateExpression(_) ->
    false.

% Generate a logical operator expression
logicalExpression(Operator, Left, Right) when ?IS_LOGICAL_OPERATOR(Operator) ->
    ?spec([{Operator, anything},
           {Left, expression},
           {Right, expression}]),
    updateRecord(expression(), [{"type", <<"LogicalExpression">>}, {"operator", Operator}, {"left", Left}, {"right", Right}]).

logicalExpression_test() ->
    ?assertEqual(logicalExpression(<<"&&">>, literal(true), literal(false)), #{
        "left" => #{"type" => <<"Literal">>,"value" => true},
        "operator" => <<"&&">>,
        "right" => #{"type" => <<"Literal">>,"value" => false},
        "type" => <<"LogicalExpression">>
    }).

is_logicalExpression(?NODETYPE(<<"LogicalExpression">>)) ->
    true;
is_logicalExpression(_) ->
    false.

% Generate a conditional expression
conditionalExpression(Test, Alternate, Consequent) ->
    ?spec([{Test, expression}, {Alternate, expression}, {Consequent, expression}]),    
    updateRecord(expression(), [{"type", <<"ConditionalExpression">>}, {"test", Test}, {"alternate", Alternate}, {"consequent", Consequent}]).

conditionalExpression_test() ->
    ?assertEqual(conditionalExpression(thisExpression(), thisExpression(), thisExpression()), #{
        "alternate" => #{"type" => <<"ThisExpression">>},
        "consequent" => #{"type" => <<"ThisExpression">>},
        "test" => #{"type" => <<"ThisExpression">>},
        "type" => <<"ConditionalExpression">>
    }).

is_conditionalExpression(?NODETYPE(<<"ConditionalExpression">>)) ->
    true;
is_conditionalExpression(_) ->
    false.

% Generate a new expression
newExpression(Callee, Arguments) ->
    ?spec([{Callee, expression}, {Arguments, expression}]),
    updateRecord(expression(), [{"type", <<"NewExpression">>}, {"callee", Callee}, {"arguments", Arguments}]).

newExpression_test() ->
    ?assertEqual(newExpression(thisExpression(), thisExpression()), #{
        "arguments" => #{"type" => <<"ThisExpression">>},
        "callee" => #{"type" => <<"ThisExpression">>},
        "type" => <<"NewExpression">>
    }).

is_newExpression(?NODETYPE(<<"NewExpression">>)) ->
    true;
is_newExpression(_) ->
    false.

% Generate a call expression
callExpression(Callee, Arguments) ->
    ?spec([{Callee, expression}, {Arguments, list_of_expression}]),
    updateRecord(expression(), [{"type", <<"CallExpression">>}, {"callee", Callee}, {"arguments", Arguments}]).

callExpression_test() ->
    ?assertEqual(callExpression(thisExpression(), [thisExpression()]), #{
        "arguments" => [#{"type" => <<"ThisExpression">>}],
        "callee" => #{"type" => <<"ThisExpression">>},
        "type" => <<"CallExpression">>
    }).

is_callExpression(?NODETYPE(<<"CallExpression">>)) ->
    true;
is_callExpression(_) ->
    false.

% Generate a member expression
memberExpression(Object, Property, Computed) ->
    ?spec([{Object, expression}, {Property, [identifier, expression]}, {Computed, boolean}]),
    updateRecord(expression(), [{"type", <<"MemberExpression">>}, {"object", Object}, {"property", Property}, {"computed", Computed}]).

memberExpression_test() ->
    ?assertEqual(memberExpression(thisExpression(), identifier("a"), false), #{
        "computed" => false,
        "object" => #{"type" => <<"ThisExpression">>},
        "property" => #{"name" => "a","type" => <<"Identifier">>},
        "type" => <<"MemberExpression">>
    }).

is_memberExpression(?NODETYPE(<<"MemberExpression">>)) ->
    true;
is_memberExpression(_) ->
    false.