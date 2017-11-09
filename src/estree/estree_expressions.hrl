% Generate a This expression
thisExpression() ->
    updateRecord(expression(), [{"type", <<"ThisExpression">>}]).

is_thisExpression(?NODETYPE(<<"ThisExpression">>)) ->
    true;
is_thisExpression(_) ->
    false.

% Generate an Array expression
arrayExpression(Elements) ->
    case {is_list(Elements), lists:all(fun(X) -> X =:= true end, lists:map(fun(X) -> is_expression(X) end, Elements))} of
        {true, true} ->
            updateRecord(expression(), [{"type", <<"ArrayExpression">>}, {"elements", Elements}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"List<Expression>">>], [{typeof(Elements), lists:map(fun(X) -> nodetype(X) end, Elements)}])
    end.


is_arrayExpression(?NODETYPE(<<"ArrayExpression">>)) ->
    true;
is_arrayExpression(_) ->
    false.

% Generate an Object expression
objectExpression(Properties) ->
    case {is_list(Properties), lists:all(fun(X) -> X =:= true end, lists:map(fun(X) -> is_property(X) end, Properties))} of
        {true, true} ->
            updateRecord(expression(), [{"type", <<"ObjectExpression">>}, {"properties", Properties}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"List<Property>">>], [{typeof(Properties), lists:map(fun(X) -> nodetype(X) end, Properties)}])
    end.

is_objectExpression(?NODETYPE(<<"ObjectExpression">>)) ->
    true;
is_objectExpression(_) ->
    false.

% Generates a Function expression
functionExpression(Identifier, Params, Body, Expression) ->
    case {lists:member(true, [is_identifier(Identifier), Identifier =:= null]), is_blockStatement(Body), is_boolean(Expression)} of
        {true, true, true} ->
            updateRecord(expression(), [{"type", <<"FunctionExpression">>}, {"id", Identifier}, {"params", Params}, {"body", Body}, {"expression", Expression}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Identifier or null">>, <<"List<Pattern(?)>">>, <<"BlockStatement">>, boolean], 
                                       [nodetype(Identifier), nodetype(Params), nodetype(Body), typeof(Expression)])
    end.

is_functionExpression(?NODETYPE(<<"FunctionExpression">>)) ->
    true;
is_functionExpression(_) ->
    false.

% A Literal Property for Object expressions
property(Key, Value) ->
    case {lists:any(fun(X) -> X =:= true end, [is_literal(Key), is_identifier(Key)]), is_expression(Value)} of
        {true, true} ->
            node("Property", [{"key", Key}, {"value", Value}, {"kind", <<"init">>}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Literal or Identifier">>, <<"Expression">>], [nodetype(Key), nodetype(Value)])
    end.

is_property(?NODETYPE(<<"Property">>)) ->
    true;
is_property(_) ->
    false.

% Generate a sequence expression
sequenceExpression(Expressions) ->
    case {is_list(Expressions), lists:all(fun(X) -> X =:= true end, lists:map(fun(X) -> is_expression(X) end, Expressions))} of
        {true, true} ->
            updateRecord(expression(), [{"type", <<"SequenceExpression">>}, {"expressions", Expressions}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"List<Expression>">>], [{typeof(Expressions)}])
    end.

is_sequenceExpression(?NODETYPE(<<"SequenceExpression">>)) ->
    true;
is_sequenceExpression(_) ->
    false.

% Generate a unary operation expression
unaryExpression(Operator, Prefix, Argument) when ?IS_UNARY_OPERATOR(Operator)  ->
    case {is_expression(Argument)} of
        {true} ->
            updateRecord(expression(), [{"type", <<"UnaryExpression">>}, {"operator", Operator}, {"prefix", Prefix}, {"argument", Argument}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Unary Operator">>, boolean, <<"Expression">>], [{nodetype(Operator), nodetype(Prefix), nodetype(Argument)}])
    end.
        
is_unaryExpression(?NODETYPE(<<"UnaryExpression">>)) ->
    true;
is_unaryExpression(_) ->
    false.

% Generate a binary operation expression
binaryExpression(Operator, Left, Right) when ?IS_BINARY_OPERATOR(Operator) ->
    case {is_expression(Left), is_expression(Right)} of
        {true, true} ->
            updateRecord(expression(), [{"type", <<"BinaryExpression">>}, {"operator", Operator}, {"left", Left}, {"right", Right}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Binary Operator">>, <<"Expression">>, <<"Expression">>], [{nodetype(Operator), nodetype(Left), nodetype(Right)}])
    end.
        
is_binaryExpression(?NODETYPE(<<"BinaryExpression">>)) ->
    true;
is_binaryExpression(_) ->
    false.

% Generate an update (increment or decrement) operator expression
updateExpression(Operator, Argument, Prefix) when ?IS_UPDATE_OPERATOR(Operator) ->
    case {is_expression(Argument)} of
        {true} ->
            updateRecord(expression(), [{"type", <<"UpdateExpression">>}, {"operator", Operator}, {"argument", Argument}, {"prefix", Prefix}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Update Operator">>, <<"Expression">>, boolean], [{nodetype(Operator), nodetype(Argument), typeof(Prefix)}])
    end.

is_updateExpression(?NODETYPE(<<"UpdateExpression">>)) ->
    true;
is_updateExpression(_) ->
    false.

% Generate a logical operator expression
logicalExpression(Operator, Left, Right) when ?IS_LOGICAL_OPERATOR(Operator) ->
    case {is_expression(Left), is_expression(Right)} of
        {true, true} ->
            updateRecord(expression(), [{"type", <<"LogicalExpression">>}, {"operator", Operator}, {"left", Left}, {"right", Right}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Logical Operator">>, <<"Expression">>, <<"Expression">>], [{nodetype(Operator), nodetype(Left), nodetype(Right)}])
    end.

is_logicalExpression(?NODETYPE(<<"LogicalExpression">>)) ->
    true;
is_logicalExpression(_) ->
    false.

% Generate a conditional expression
conditionalExpression(Test, Alternate, Consequent) ->
    case {is_expression(Test), is_expression(Alternate), is_expression(Consequent)} of
        {true, true, true} ->
            updateRecord(expression(), [{"type", <<"ConditionalExpression">>}, {"test", Test}, {"alternate", Alternate}, {"consequent", Consequent}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Expression">>, <<"Expression">>, <<"Expression">>], [{nodetype(Test), nodetype(Alternate), nodetype(Consequent)}])
    end.

is_conditionalExpression(?NODETYPE(<<"ConditionalExpression">>)) ->
    true;
is_conditionalExpression(_) ->
    false.

% Generate a new expression
newExpression(Callee, Arguments) ->
    case {is_expression(Callee), is_list(Arguments), lists:all(fun(X) -> X =:= true end, lists:map(fun(X) -> is_expression(X) end, Arguments))} of
        {true, true, true} ->
            updateRecord(expression(), [{"type", <<"NewExpression">>}, {"callee", Callee}, {"arguments", Arguments}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Expression">>, <<"Expression">>], [{nodetype(Callee), nodetype(Arguments)}])
    end.

is_newExpression(?NODETYPE(<<"NewExpression">>)) ->
    true;
is_newExpression(_) ->
    false.

% Generate a call expression
callExpression(Callee, Arguments) ->
    case {is_expression(Callee), is_list(Arguments), lists:all(fun(X) -> X =:= true end, lists:map(fun(X) -> is_expression(X) end, Arguments))} of
        {true, true, true} ->
            updateRecord(expression(), [{"type", <<"CallExpression">>}, {"callee", Callee}, {"arguments", Arguments}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Expression">>, <<"Expression">>], [{nodetype(Callee), nodetype(Arguments)}])
    end.

is_callExpression(?NODETYPE(<<"CallExpression">>)) ->
    true;
is_callExpression(_) ->
    false.

% Generate a member expression
memberExpression(Object, Property, Computed) ->
    case {is_identifier(Object), lists:member(true, [is_identifier(Property), is_expression(Property)]), is_boolean(Computed)} of
        {true, true, true} ->
            updateRecord(expression(), [{"type", <<"MemberExpression">>}, {"object", Object}, {"property", Property}, {"computed", Computed}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Expression">>, <<"Identifier or Expression">>, boolean], [nodetype(Object), nodetype(Property), typeof(Computed)])
    end.
    
is_memberExpression(?NODETYPE(<<"MemberExpression">>)) ->
    true;
is_memberExpression(_) ->
    false.