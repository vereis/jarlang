-define(IS_UNARY_OPERATOR(X), 
        X =:= <<"-">> ;      X =:= <<"+">> ;    X =:= <<"!">> ; X =:= <<"~">> ; 
        X =:= <<"typeof">> ; X =:= <<"void">> ; X =:= <<"delete">>).

-define(IS_BINARY_OPERATOR(X),
        X =:= <<"==">> ; X =:= <<"!=">> ; X =:= <<"===">> ; X =:= <<"!==">> ;
        X =:= <<"<">>  ; X =:= <<">">>  ; X =:= <<"<=">>  ; X =:= <<">=">> ;
        X =:= <<"<<">> ; X =:= <<">>">> ; X =:= <<">>>">> ; X =:= <<"+">> ;
        X =:= <<"-">>  ; X =:= <<"*">>  ; X =:= <<"/">>   ; X =:= <<"%">> ;
        X =:= <<"|">>  ; X =:= <<"^">>  ; X =:= <<"&">>   ; X =:= <<"in">> ;
        X =:= <<"instanceof">> ; X =:= <<"..">>).

-define(IS_LOGICAL_OPERATOR(X),
        X =:= <<"||">> ; X =:= <<"&&">>).

-define(IS_ASSIGNMENT_OPERATOR(X),
        X =:= <<"=">>    ; X =:= <<"+=">> ; X =:= <<"-=">>  ; X =:= <<"*=">> ; 
        X =:= <<"/=">>   ; X =:= <<"%=">> ; X =:= <<"<<=">> ; X =:= <<">>=">> ;
        X =:= <<">>>=">> ; X =:= <<"|=">> ; X =:= <<"^=">>  ; X =:= <<"&=">>).

-define(IS_UPDATE_OPERATOR(X),
        X =:= <<"++">> ; X =:= <<"--">>).

% Generate a This expression
thisExpression() ->
    updateRecord(expression(), [{"type", <<"ThisExpression">>}]).

is_thisExpression(?NODETYPE(<<"ThisExpression">>)) ->
    true;
is_thisExpression(_) ->
    false.

% Generate an Array expression
arrayExpression(Elements) when is_list(Elements) ->
    updateRecord(expression(), [{"type", <<"ArrayExpression">>}, {"elements", Elements}]);
arrayExpression(Elements) ->
    arrayExpression([Elements]).

is_arrayExpression(?NODETYPE(<<"ArrayExpression">>)) ->
    true;
is_arrayExpression(_) ->
    false.

% Generate an Object expression
objectExpression(Properties) when is_list(Properties) ->
    updateRecord(expression(), [{"type", <<"ObjectExpression">>}, {"properties", Properties}]);
objectExpression(Properties) ->
    objectExpression([Properties]).

is_objectExpression(?NODETYPE(<<"ObjectExpression">>)) ->
    true;
is_objectExpression(_) ->
    false.

% Generates a Function expression
functionExpression(Identifier, Params, Body, Expression) when is_list(Params) ->
    updateRecord(expression(), [{"type", <<"FunctionExpression">>}, {"id", Identifier}, {"params", Params}, {"body", Body}, {"expression", Expression}]);
functionExpression(Identifier, Params, Body, Expression) ->
    functionExpression(Identifier, [Params], Body, Expression).

is_functionExpression(?NODETYPE(<<"FunctionExpression">>)) ->
    true;
is_functionExpression(_) ->
    false.

% A Literal Property for Object expressions
property(Key, Value) ->
    node("Property", [{"key", Key}, {"value", Value}, {"kind", <<"init">>}]).

is_property(?NODETYPE(<<"Property">>)) ->
    true;
is_property(_) ->
    false.

% Generate a sequence expression
sequenceExpression(Expressions) when length(Expressions) > 1, is_list(Expressions) ->
    updateRecord(expression(), [{"type", <<"SequenceExpression">>}, {"expressions", Expressions}]).

is_sequenceExpression(?NODETYPE(<<"SequenceExpression">>)) ->
    true;
is_sequenceExpression(_) ->
    false.

% Generate a unary operation expression
unaryExpression(Operator, Prefix, Argument) when ?IS_UNARY_OPERATOR(Operator)  ->
    updateRecord(expression(), [{"type", <<"UnaryExpression">>}, {"operator", Operator}, {"prefix", Prefix}, {"argument", Argument}]).

is_unaryExpression(?NODETYPE(<<"UnaryExpression">>)) ->
    true;
is_unaryExpression(_) ->
    false.

% Generate a binary operation expression
binaryExpression(Operator, Left, Right) when ?IS_BINARY_OPERATOR(Operator) ->
    updateRecord(expression(), [{"type", <<"BinaryExpression">>}, {"operator", Operator}, {"left", Left}, {"right", Right}]).

is_binaryExpression(?NODETYPE(<<"BinaryExpression">>)) ->
    true;
is_binaryExpression(_) ->
    false.

% Generate an update (increment or decrement) operator expression
updateExpression(Operator, Argument, Prefix) when ?IS_UPDATE_OPERATOR(Operator) ->
    updateRecord(expression(), [{"type", <<"UpdateExpression">>}, {"operator", Operator}, {"argument", Argument}, {"prefix", Prefix}]).

is_updateExpression(?NODETYPE(<<"UpdateExpression">>)) ->
    true;
is_updateExpression(_) ->
    false.

% Generate a logical operator expression
logicalExpression(Operator, Left, Right) when ?IS_LOGICAL_OPERATOR(Operator) ->
    updateRecord(expression(), [{"type", <<"LogicalExpression">>}, {"operator", Operator}, {"left", Left}, {"right", Right}]).

is_logicalExpression(?NODETYPE(<<"LogicalExpression">>)) ->
    true;
is_logicalExpression(_) ->
    false.

% Generate a conditional expression
conditionalExpression(Test, Alternate, Consequent) ->
    updateRecord(expression(), [{"type", <<"ConditionalExpression">>}, {"test", Test}, {"alternate", Alternate}, {"consequent", Consequent}]).

is_conditionalExpression(?NODETYPE(<<"ConditionalExpression">>)) ->
    true;
is_conditionalExpression(_) ->
    false.

% Generate a new expression
newExpression(Callee, Arguments) ->
    updateRecord(expression(), [{"type", <<"NewExpression">>}, {"callee", Callee}, {"arguments", Arguments}]).

is_newExpression(?NODETYPE(<<"NewExpression">>)) ->
    true;
is_newExpression(_) ->
    false.

% Generate a call expression
callExpression(Callee, Arguments) ->
    updateRecord(expression(), [{"type", <<"CallExpression">>}, {"callee", Callee}, {"arguments", Arguments}]).

is_callExpression(?NODETYPE(<<"CallExpression">>)) ->
    true;
is_callExpression(_) ->
    false.

% Generate a member expression
memberExpression(Object, Property, Computed) ->
    updateRecord(expression(), [{"type", <<"MemberExpression">>}, {"object", Object}, {"property", Property}, {"computed", Computed}]).
    
is_memberExpression(?NODETYPE(<<"MemberExpression">>)) ->
    true;
is_memberExpression(_) ->
    false.