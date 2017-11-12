% Generate a This expression
thisExpression() ->
    updateRecord(expression(), [{"type", <<"ThisExpression">>}]).

is_thisExpression(?NODETYPE(<<"ThisExpression">>)) ->
    true;
is_thisExpression(_) ->
    false.

% Generate an Array expression
arrayExpression(Elements) ->
    ?spec([{Elements, list_of_expression}]),
    updateRecord(expression(), [{"type", <<"ArrayExpression">>}, {"elements", Elements}]).

is_arrayExpression(?NODETYPE(<<"ArrayExpression">>)) ->
    true;
is_arrayExpression(_) ->
    false.

% Generate an Object expression
objectExpression(Properties) ->
    ?spec([{Properties, list_of_property}]),
    updateRecord(expression(), [{"type", <<"ObjectExpression">>}, {"properties", Properties}]).

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

is_functionExpression(?NODETYPE(<<"FunctionExpression">>)) ->
    true;
is_functionExpression(_) ->
    false.

% A Literal Property for Object expressions
property(Key, Value) ->
    ?spec([{Key, [literal, identifier]}, {Value, expression}]),
    node("Property", [{"key", Key}, {"value", Value}, {"kind", <<"init">>}]).

is_property(?NODETYPE(<<"Property">>)) ->
    true;
is_property(_) ->
    false.

% Generate a sequence expression
sequenceExpression(Expressions) ->
    ?spec([{Expressions, list_of_expression}]),
    updateRecord(expression(), [{"type", <<"SequenceExpression">>}, {"expressions", Expressions}]).

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

is_logicalExpression(?NODETYPE(<<"LogicalExpression">>)) ->
    true;
is_logicalExpression(_) ->
    false.

% Generate a conditional expression
conditionalExpression(Test, Alternate, Consequent) ->
    ?spec([{Test, expression}, {Alternate, expression}, {Consequent, expression}]),    
    updateRecord(expression(), [{"type", <<"ConditionalExpression">>}, {"test", Test}, {"alternate", Alternate}, {"consequent", Consequent}]).

is_conditionalExpression(?NODETYPE(<<"ConditionalExpression">>)) ->
    true;
is_conditionalExpression(_) ->
    false.

% Generate a new expression
newExpression(Callee, Arguments) ->
    ?spec([{Callee, expression}, {Arguments, expression}]),
    updateRecord(expression(), [{"type", <<"NewExpression">>}, {"callee", Callee}, {"arguments", Arguments}]).

is_newExpression(?NODETYPE(<<"NewExpression">>)) ->
    true;
is_newExpression(_) ->
    false.

% Generate a call expression
callExpression(Callee, Arguments) ->
    ?spec([{Callee, expression}, {Arguments, list_of_expression}]),
    updateRecord(expression(), [{"type", <<"CallExpression">>}, {"callee", Callee}, {"arguments", Arguments}]).

is_callExpression(?NODETYPE(<<"CallExpression">>)) ->
    true;
is_callExpression(_) ->
    false.

% Generate a member expression
memberExpression(Object, Property, Computed) ->
    ?spec([{Object, expression}, {Property, [identifier, expression]}, {Computed, boolean}]),
    updateRecord(expression(), [{"type", <<"MemberExpression">>}, {"object", Object}, {"property", Property}, {"computed", Computed}]).
    
is_memberExpression(?NODETYPE(<<"MemberExpression">>)) ->
    true;
is_memberExpression(_) ->
    false.