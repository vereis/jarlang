%%% Erlang implementation of the MDN documentation for the JavaScript AST which you can find here:
%%% https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Parser_API

-module(estree).
-author(["Chris Bailey"]).

-define(VERSION, "2.0.0").

-vsn(?VERSION).

-compile({no_auto_import, [node/0, node/1]}).

%% Export all functions if test build
-ifdef(TEST).
    -compile([{no_auto_import, [node/1, node/0]}, export_all]).
-else.
    -export([
        to_map/1,
        to_list/1,

        position/2,
        sourceLocation/3,
        addLocationData/4,

        declaration/0,
        expression/0,
        statement/0,

        identifier/1,
        literal/1,
        program/1,

        spreadElement/1,

        variableDeclaration/2,
        variableDeclarator/2,
        functionDeclaration/3,

        thisExpression/0,
        arrayExpression/1,
        objectExpression/1,
        property/2,
        functionExpression/4,
        sequenceExpression/1,
        unaryExpression/3,
        assignmentExpression/3,
        binaryExpression/3,
        updateExpression/3,
        logicalExpression/3,
        conditionalExpression/3,
        newExpression/2,
        callExpression/2,
        memberExpression/3,

        emptyStatement/0,
        expressionStatement/1,
        blockStatement/1,
        ifStatement/3,
        labeledStatement/2,
        breakStatement/1,
        continueStatement/1,
        withStatement/2,
        switchStatement/3,
        switchCase/2,
        returnStatement/1,
        throwStatement/1,
        tryStatement/4,
        catchClause/3,
        whileStatement/2,
        forStatement/4,
        forInStatement/4,
        forOfStatement/3,

        error/3,
        useStrict/0,
        constDeclaration/2,
        varDeclaration/2,
        letDeclaration/2
    ]).
-endif.





%%% ---------------------------------------------------------------------------------------------%%%
%%% - TYPE DEFINITIONS --------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Node composition
-type es_literal()    :: nonempty_string()
                       | iolist()
                       | binary()
                       | 'null'
                       | 'undefined'
                       | integer()
                       | float()
                       | boolean().

-type es_identifier() :: nonempty_string()
                       | iolist().

-type es_node()        :: {'__estree_node', es_node_type(), PropertyFields::es_node_fields()}.

-type es_node_type()   :: atom().

-type es_node_field()  :: {Key::es_literal(), Value::any()}.

-type es_node_fields() :: [es_node_field()].

%% Primitives
-type tba_node()             :: {'__estree_node', '__tba', PropertyFields::es_node_fields()}.

-type identifier_node()      :: {'__estree_node', 'Identifier', PropertyFields::es_node_fields()}.

-type source_location_node() :: {'__estree_node', 'SourceLocation', PropertyFields::es_node_fields()}.

-type position_node()        :: {'__estree_node', 'Position', PropertyFields::es_node_fields()}.

-type literal_node()         :: {'__estree_node', 'Literal', PropertyFields::es_node_fields()}.

-type program_node()         :: {'__estree_node', 'Program', PropertyFields::es_node_fields()}.

-type spread_element_node()  :: {'__estree_node', 'SpreadElement', PropertyFields::es_node_fields()}.

-type declaration_node()     :: {'__estree_node', 'Declaration', PropertyFields::es_node_fields()}
                              | variable_declaration_node()
                              | function_declaration_node()
                              | statement_node().

-type expression_node()      :: {'__estree_node', 'Expression', PropertyFields::es_node_fields()}
                              | this_expression_node()
                              | array_expression_node()
                              | object_expression_node()
                              | function_expression_node()
                              | sequence_expression_node()
                              | unary_expression_node()
                              | assignment_expression_node()
                              | binary_expression_node()
                              | update_expression_node()
                              | logical_expression_node()
                              | conditional_expression_node()
                              | new_expression_node()
                              | call_expression_node()
                              | member_expression_node().

-type statement_node()       :: {'__estree_node', 'Statement', PropertyFields::es_node_fields()}
                              | empty_statement_node()
                              | expression_statement_node()
                              | block_statement_node()
                              | if_statement_node()
                              | labeled_statement_node()
                              | break_statement_node()
                              | continue_statement_node()
                              | with_statement_node()
                              | switch_statement_node()
                              | return_statement_node()
                              | throw_statement_node()
                              | try_statement_node()
                              | while_statement_node()
                              | for_statement_node()
                              | for_in_statement_node()
                              | for_of_statement_node().

%% Declarations
-type variable_declaration_node() :: {'__estree_node', 'VariableDeclaration', PropertyFields::es_node_fields()}.

-type variable_declarator_node()  :: {'__estree_node', 'VariableDeclarator', PropertyFields::es_node_fields()}.

-type function_declaration_node() :: {'__estree_node', 'FunctionDeclaration', PropertyFields::es_node_fields()}.

%% Expressions
-type this_expression_node()        :: {'__estree_node', 'ThisExpression', PropertyFields::es_node_fields()}.

-type array_expression_node()       :: {'__estree_node', 'ArrayExpression', PropertyFields::es_node_fields()}.

-type object_expression_node()      :: {'__estree_node', 'ObjectExpression', PropertyFields::es_node_fields()}.

-type object_property_node()        :: {'__estree_node', 'Property', PropertyFields::es_node_fields()}.

-type function_expression_node()    :: {'__estree_node', 'FunctionExpression', PropertyFields::es_node_fields()}.

-type sequence_expression_node()    :: {'__estree_node', 'SequenceExpression', PropertyFields::es_node_fields()}.

-type unary_expression_node()       :: {'__estree_node', 'UnaryExpression', PropertyFields::es_node_fields()}.

-type assignment_expression_node()  :: {'__estree_node', 'AssignmentExpression', PropertyFields::es_node_fields()}.

-type binary_expression_node()      :: {'__estree_node', 'BinaryExpression', PropertyFields::es_node_fields()}.

-type update_expression_node()      :: {'__estree_node', 'UpdateExpression', PropertyFields::es_node_fields()}.

-type logical_expression_node()     :: {'__estree_node', 'LogicalExpression', PropertyFields::es_node_fields()}.

-type conditional_expression_node() :: {'__estree_node', 'ConditionalExpression', PropertyFields::es_node_fields()}.

-type new_expression_node()         :: {'__estree_node', 'NewExpression', PropertyFields::es_node_fields()}.

-type call_expression_node()        :: {'__estree_node', 'CallExpression', PropertyFields::es_node_fields()}.

-type member_expression_node()      :: {'__estree_node', 'MemberExpression', PropertyFields::es_node_fields()}.

%% Statements
-type empty_statement_node()       :: {'__estree_node', 'EmptyStatement', PropertyFields::es_node_fields()}.

-type expression_statement_node()  :: {'__estree_node', 'ExpressionStatement', PropertyFields::es_node_fields()}.

-type block_statement_node()       :: {'__estree_node', 'BlockStatement', PropertyFields::es_node_fields()}.

-type if_statement_node()          :: {'__estree_node', 'IfStatement', PropertyFields::es_node_fields()}.

-type labeled_statement_node()     :: {'__estree_node', 'LabeledStatement', PropertyFields::es_node_fields()}.

-type break_statement_node()       :: {'__estree_node', 'BreakStatement', PropertyFields::es_node_fields()}.

-type continue_statement_node()    :: {'__estree_node', 'ContinueStatement', PropertyFields::es_node_fields()}.

-type with_statement_node()        :: {'__estree_node', 'WithStatement', PropertyFields::es_node_fields()}.

-type switch_statement_node()      :: {'__estree_node', 'SwitchStatement', PropertyFields::es_node_fields()}.

-type switch_case_statement_node() :: {'__estree_node', 'SwitchCase', PropertyFields::es_node_fields()}.

-type return_statement_node()      :: {'__estree_node', 'ReturnStatement', PropertyFields::es_node_fields()}.

-type throw_statement_node()       :: {'__estree_node', 'ThrowStatement', PropertyFields::es_node_fields()}.

-type try_statement_node()         :: {'__estree_node', 'TryStatement', PropertyFields::es_node_fields()}.

-type catch_clause_node()          :: {'__estree_node', 'CatchClause', PropertyFields::es_node_fields()}.

-type while_statement_node()       :: {'__estree_node', 'WhileStatement', PropertyFields::es_node_fields()}.

-type for_statement_node()         :: {'__estree_node', 'ForStatement', PropertyFields::es_node_fields()}.

-type for_in_statement_node()      :: {'__estree_node', 'ForInStatement', PropertyFields::es_node_fields()}.

-type for_of_statement_node()      :: {'__estree_node', 'ForOfStatement', PropertyFields::es_node_fields()}.


%%% ---------------------------------------------------------------------------------------------%%%
%%% - GUARD MACROS ------------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Macros for use in guards in certain places
-define(IS_LITERAL(X),
    is_list(X) ; is_binary(X) ; X =:= null ; X =:= undefined ; is_integer(X) ; is_float(X) ; is_boolean(X)).

-define(IS_IDENTIFIER(X),
    is_list(X) ; is_binary(X)).

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





%%% ---------------------------------------------------------------------------------------------%%%
%%% - UTILITY MACROS & FUNCTIONS ----------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Generates a node datastructure which has yet to be given type information.
-spec node() -> tba_node().
node() ->
    node('__tba').

%% Generates a node data structure of a given type with no additional parameters
-spec node(es_identifier() | atom()) -> es_node().
node(T) ->
    node(T, []).

%% Generates a node data structure of a given type while also setting additional parameters
-spec node(es_identifier() | atom(),
           es_node_fields()) -> es_node().
node(T, Fs) when is_atom(T) ->
    NewNode = {'__estree_node', T, [{"type", list_to_binary(atom_to_list(T))}]},
    updateRecord(NewNode, Fs);
node(T, Fs) when is_list(T) ->
    node(list_to_atom(T), Fs).

%% Updates a given node with the given NodeFields
-spec updateRecord(es_node(),
                   es_node_fields()) -> es_node().
updateRecord({_, _, NodeFields}, NewFields) ->
    UpdatedNodeFields = merge_node_fields(NodeFields, NewFields),
    {_, NodeType} = lists:keyfind("type", 1, UpdatedNodeFields),
    {'__estree_node', list_to_atom(binary_to_list(NodeType)), UpdatedNodeFields}.

%% Merges two node fields
-spec merge_node_fields(es_node_fields(),
                        es_node_fields()) -> es_node_fields().
merge_node_fields(Old, New) ->
    UniqueToOld = lists:filter(fun({K, _V}) -> not lists:keymember(K, 1, New) end, Old),
    New ++ UniqueToOld.

%% Add location data metadata to any node
-spec addLocationData(es_node(),
                      pos_integer(),
                      non_neg_integer(),
                      non_neg_integer()) -> es_node().
addLocationData(Node, LineNumber, ColStart, ColEnd) ->
    updateRecord(Node, [{"loc", sourceLocation(LineNumber, ColStart, ColEnd)}]).

%% Converts ESTREE nodes into an erlang map
-spec to_map(es_node()) -> map().
to_map({'__estree_node', _, Params}) ->
    maps:from_list([{K, to_map(V)} || {K, V} <- Params]);
to_map([{'__estree_node', _, _} | _] = X) ->
    [to_map(Y) || Y <- X];
to_map(X) ->
    %io:format("ESTREE_TO_MAP fail: ~p~n~n", [X]),
    X.

-spec to_list(es_node()) -> list().
to_list({'__estree_node', _, Params}) ->
    [{K, to_list(V)} || {K, V} <- Params];
to_list([{'__estree_node', _, _} | _] = X) ->
    [to_list(Y) || Y <- X];
to_list(X) ->
    %io:format("ESTREE_TO_MAP fail: ~p~n~n", [X]),
    X.

%%% ---------------------------------------------------------------------------------------------%%%
%%% - AST PRIMITIVES ----------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Generates an Identifier Node
-spec identifier(es_identifier()) -> identifier_node().
identifier(Name) when ?IS_IDENTIFIER(Name) ->
    node("Identifier", [{"name", Name}]).

%% Generates a SourceLocation node
-spec sourceLocation(pos_integer(),
                     non_neg_integer(),
                     non_neg_integer()) -> source_location_node().
sourceLocation(LineNumber, ColStart, ColEnd) ->
    %?spec([{LineNumber, integer}, {ColStart, integer}, {ColEnd, integer}]),
    node("SourceLocation", [{"source", null},
                            {"start", position(LineNumber, ColStart)},
                            {"end", position(LineNumber, ColEnd)}]).

%% Generates a Position node
-spec position(pos_integer(),
               non_neg_integer()) -> position_node().
position(Line, Col) ->
    %?spec([{Line, integer}, {Col, integer}]),
    node("Position", [{"line", Line},
                      {"column", Col}]).

%% Generates a Literal Node
-spec literal(es_literal()) -> literal_node().
literal(Value) when ?IS_LITERAL(Value) ->
    node("Literal", [{"value", Value}]).

%% Generates a Program Node
-spec program([statement_node()]) -> program_node().
program(Statements) ->
    %?spec([{Statements, list_of_statement}]),
    node("Program", [{"body", Statements}]).

%% Generates a generic Declaration Node
-spec declaration() -> declaration_node().
declaration() ->
    node('Declaration').

%% Generate a generic Expression Node
-spec expression() -> expression_node().
expression() ->
    node('Expression').

%% Generates a generic Statement Node
-spec statement() -> statement_node().
statement() ->
    node('Statement').

%% SpreadExpression
-spec spreadElement([identifier_node() | array_expression_node()]) -> spread_element_node().
spreadElement(Argument) ->
    %?spec([{Argument, [identifier, arrayExpression]}]),
    updateRecord(node(), [{"type", <<"SpreadElement">>},
                          {"argument", Argument}]).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - DECLARATION NODES -------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Generates a variable declaration
-spec variableDeclaration([variable_declarator_node()],
                          es_literal()) -> variable_declaration_node().
variableDeclaration(Declarations, Kind)  ->
    %?spec([{Declarations, list_of_variableDeclarator}, {Kind, [variableType]}]),
    updateRecord(declaration(), [{"type", <<"VariableDeclaration">>},
                                 {"declarations", Declarations},
                                 {"kind", Kind}]).

%% Generates a variable declarator
-spec variableDeclarator(identifier_node(),
                         expression_node()) -> variable_declarator_node().
variableDeclarator(Identifier, Init) ->
    %?spec([{Identifier, identifier}, {Init, expression}]),
    node("VariableDeclarator", [{"id", Identifier},
                                {"init", Init}]).

%% Generate a function declaration
-spec functionDeclaration(identifier_node(),
                          [identifier_node()],
                          block_statement_node()) -> function_declaration_node().
functionDeclaration(Identifier, Params, Body) ->
    %?spec([{Identifier, identifier}, {Params, list_of_identifier}, {Body, [expression, blockStatement]}]),
    updateRecord(declaration(), [{"type", <<"FunctionDeclaration">>},
                                 {"id", Identifier},
                                 {"params", Params},
                                 {"body", Body}]).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - EXPRESSION NODES --------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Generate a This expression
-spec thisExpression() -> this_expression_node().
thisExpression() ->
    updateRecord(expression(), [{"type", <<"ThisExpression">>}]).

%% Generate an Array expression
-spec arrayExpression([expression_node()]) -> array_expression_node().
arrayExpression(Elements) ->
    %?spec([{Elements, list_of_expression}]),
    updateRecord(expression(), [{"type", <<"ArrayExpression">>},
                                {"elements", Elements}]).

%% Generate an Object expression
-spec objectExpression([object_property_node()]) -> object_expression_node().
objectExpression(Properties) ->
    %?spec([{Properties, list_of_property}]),
    updateRecord(expression(), [{"type", <<"ObjectExpression">>},
                                {"properties", Properties}]).

%% A Literal Property for Object expressions
-spec property(literal_node() | identifier_node(),
               expression_node()) -> object_property_node().
property(Key, Value) ->
    %?spec([{Key, [literal, identifier]}, {Value, expression}]),
    node("Property", [{"key", Key},
                      {"value", Value},
                      {"kind", <<"init">>}]).

%% Generates a Function expression
-spec functionExpression(identifier_node() | 'null',
                         [identifier_node()],
                         block_statement_node(),
                         boolean()) -> function_expression_node().
functionExpression(Identifier, Params, Body, Expression) ->
    %?spec([{Identifier, [identifier, null]},
    %       {Params, list_of_identifier},
    %       {Body, blockStatement},
    %       {Expression, boolean}]),
    updateRecord(expression(), [{"type", <<"FunctionExpression">>},
                                {"id", Identifier},
                                {"params", Params},
                                {"body", Body},
                                {"expression", Expression}]).

%% Generate a sequence expression
-spec sequenceExpression([expression_node()]) -> sequence_expression_node().
sequenceExpression(Expressions) ->
    %?spec([{Expressions, list_of_expression}]),
    updateRecord(expression(), [{"type", <<"SequenceExpression">>},
                                {"expressions", Expressions}]).

%% Generate a unary operation expression
-spec unaryExpression(any(),
                      boolean(),
                      expression_node()) -> unary_expression_node().
unaryExpression(Operator, Prefix, Argument) when ?IS_UNARY_OPERATOR(Operator)  ->
    %?spec([{Operator, anything},
    %       {Prefix, boolean},
    %       {Argument, expression}]),
    updateRecord(expression(), [{"type", <<"UnaryExpression">>},
                                {"operator", Operator},
                                {"prefix", Prefix},
                                {"argument", Argument}]).

%% Generate an assignment expression
-spec assignmentExpression(any(),
                           expression_node(),
                           expression_node()) -> assignment_expression_node().
assignmentExpression(Operator, Left, Right) ->
    %?spec([{Operator, anything},
    %       {Left, expression},
    %       {Right, expression}]),
    updateRecord(expression(), [{"type", <<"AssignmentExpression">>},
                                {"operator", Operator},
                                {"left", Left},
                                {"right", Right}]).

%% Generate a binary operation expression
-spec binaryExpression(any(),
                       expression_node(),
                       expression_node()) -> binary_expression_node().
binaryExpression(Operator, Left, Right) when ?IS_BINARY_OPERATOR(Operator) ->
    %?spec([{Operator, anything},
    %       {Left, expression},
    %       {Right, expression}]),
    updateRecord(expression(), [{"type", <<"BinaryExpression">>},
                                {"operator", Operator},
                                {"left", Left},
                                {"right", Right}]).

%% Generate an update (increment or decrement) operator expression
-spec updateExpression(any(),
                       expression_node(),
                       boolean()) -> update_expression_node().
updateExpression(Operator, Argument, Prefix) when ?IS_UPDATE_OPERATOR(Operator) ->
    %?spec([{Operator, anything},
    %       {Argument, expression},
    %       {Prefix, boolean}]),
    updateRecord(expression(), [{"type", <<"UpdateExpression">>},
                                {"operator", Operator},
                                {"argument", Argument},
                                {"prefix", Prefix}]).

%% Generate a logical operator expression
-spec logicalExpression(any(),
                        expression_node(),
                        expression_node()) -> logical_expression_node().
logicalExpression(Operator, Left, Right) when ?IS_LOGICAL_OPERATOR(Operator) ->
    %?spec([{Operator, anything},
    %       {Left, expression},
    %       {Right, expression}]),
    updateRecord(expression(), [{"type", <<"LogicalExpression">>},
                                {"operator", Operator},
                                {"left", Left},
                                {"right", Right}]).

%% Generate a conditional expression
-spec conditionalExpression(expression_node(),
                            expression_node(),
                            expression_node()) -> conditional_expression_node().
conditionalExpression(Test, Alternate, Consequent) ->
    %?spec([{Test, expression}, {Alternate, expression}, {Consequent, expression}]),
    updateRecord(expression(), [{"type", <<"ConditionalExpression">>},
                                {"test", Test},
                                {"alternate", Alternate},
                                {"consequent", Consequent}]).

%% Generate a new expression
-spec newExpression(expression_node(),
                    [expression_node()]) -> new_expression_node().
newExpression(Callee, Arguments) ->
    %?spec([{Callee, expression}, {Arguments, list_of_expression}]),
    updateRecord(expression(), [{"type", <<"NewExpression">>},
                                {"callee", Callee},
                                {"arguments", Arguments}]).

%% Generate a call expression
-spec callExpression(expression_node(),
                     [expression_node()]) -> call_expression_node().
callExpression(Callee, Arguments) ->
    %?spec([{Callee, expression}, {Arguments, list_of_expression}]),
    updateRecord(expression(), [{"type", <<"CallExpression">>},
                                {"callee", Callee},
                                {"arguments", Arguments}]).

%% Generate a member expression
-spec memberExpression(expression_node(),
                       identifier_node() | expression_node(),
                       boolean()) -> member_expression_node().
memberExpression(Object, Property, Computed) ->
    %?spec([{Object, expression}, {Property, [identifier, expression]}, {Computed, boolean}]),
    updateRecord(expression(), [{"type", <<"MemberExpression">>},
                                {"object", Object},
                                {"property", Property},
                                {"computed", Computed}]).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - STATEMENT NODES ---------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Generates an empty statement node - i.e. an empty semicolon
-spec emptyStatement() -> empty_statement_node().
emptyStatement() ->
    updateRecord(statement(), [{"type", <<"EmptyStatement">>}]).

%% Generates an Expression Statement
-spec expressionStatement(expression_node()) -> expression_statement_node().
expressionStatement(Expression) ->
    updateRecord(statement(), [{"type", <<"ExpressionStatement">>},
                               {"expression", Expression}]).

%% Generates a block statement
-spec blockStatement([statement_node()]) -> block_statement_node().
blockStatement(Body) ->
    %?spec([{Body, list_of_statement}]),
    updateRecord(statement(), [{"type", <<"BlockStatement">>},
                               {"body", Body}]).

%% Generates an if statement
-spec ifStatement(expression_node(),
                  statement_node(),
                  statement_node() | 'null') -> if_statement_node().
ifStatement(Test, Consequent, Alternate) ->
    %?spec([{Test, expression}, {Consequent, statement}, {Alternate, [statement, null]}]),
    updateRecord(statement(), [{"type", <<"IfStatement">>},
                               {"test", Test},
                               {"consequent", Consequent},
                               {"alternate", Alternate}]).

%% Generates a Labeled statement (prefixed with break/continue)
-spec labeledStatement(identifier_node(),
                       statement_node()) -> labeled_statement_node().
labeledStatement(Identifier, Body) ->
    %?spec([{Identifier, identifier}, {Body, statement}]),
    updateRecord(statement(), [{"type", <<"LabeledStatement">>},
                               {"label", Identifier},
                               {"body", Body}]).

%% Generates a break statement
-spec breakStatement(identifier_node() | 'null') -> break_statement_node().
breakStatement(Identifier) ->
    %?spec([{Identifier, [null, identifier]}]),
    updateRecord(statement(), [{"type", <<"BreakStatement">>},
                               {"label", Identifier}]).

%% Generates a continue statement
-spec continueStatement(identifier_node() | 'null') -> continue_statement_node().
continueStatement(Identifier) ->
    %?spec([{Identifier, [null, identifier]}]),
    updateRecord(statement(), [{"type", <<"ContinueStatement">>},
                               {"label", Identifier}]).

%% Generates a with statement
-spec withStatement(expression_node(),
                    block_statement_node()) -> with_statement_node().
withStatement(Expression, BlockStatement) ->
    %?spec([{Expression, expression}, {BlockStatement, blockStatement}]),
    updateRecord(statement(), [{"type", <<"WithStatement">>},
                               {"object", Expression},
                               {"body", BlockStatement}]).

%% Generates a switch statement
-spec switchStatement(expression_node(),
                      [switch_case_statement_node()],
                      boolean()) -> switch_statement_node().
switchStatement(Expression, Cases, HasLexScope) ->
    %?spec([{Expression, expression}, {Cases, list_of_switchCase}, {HasLexScope, boolean}]),
    updateRecord(statement(), [{"type", <<"SwitchStatement">>},
                               {"discriminant", Expression},
                               {"cases", Cases},
                               {"lexical", HasLexScope}]).

%% Generates a case for use in a switch statement
-spec switchCase(expression_node(),
                 [statement_node()]) -> switch_case_statement_node().
switchCase(Test, Consequent) ->
    %?spec([{Test, expression}, {Consequent, list_of_statement}]),
    updateRecord(statement(), [{"type", <<"SwitchCase">>},
                               {"test", Test},
                               {"consequent", Consequent}]).

%% Generates a return statement
-spec returnStatement(expression_node() | 'null') -> return_statement_node().
returnStatement(Expression) ->
    %?spec([{Expression, [null, expression]}]),
    updateRecord(statement(), [{"type", <<"ReturnStatement">>},
                               {"argument", Expression}]).

%% Generates a throw statement
-spec throwStatement(expression_node() | 'null') -> throw_statement_node().
throwStatement(Expression) ->
    %?spec([{Expression, [null, expression]}]),
    updateRecord(statement(), [{"type", <<"ThrowStatement">>},
                               {"argument", Expression}]).

%% Generates a try statement
-spec tryStatement(block_statement_node(),
                   catch_clause_node(),
                   [catch_clause_node()],
                   block_statement_node()) -> try_statement_node().
tryStatement(BlockStatement, Handler, GuardedHandler, Finalizer) ->
    %?spec([{BlockStatement, blockStatement},
    %       {Handler, catchClause},
    %       {GuardedHandler, list_of_catchClause},
    %       {Finalizer, blockStatement}]),
    updateRecord(statement(), [{"type", <<"TryStatement">>},
                               {"block", BlockStatement},
                               {"handler", Handler},
                               {"guardedHandler", GuardedHandler},
                               {"finalizer", Finalizer}]).

%% Generates a catch clause for use in a try statement
-spec catchClause(identifier_node(),
                  expression_node(),
                  block_statement_node()) -> catch_clause_node().
catchClause(Param, Guard, Body) ->
    %?spec([{Param, identifier}, {Guard, expression}, {Body, blockStatement}]),
    updateRecord(statement(), [{"type", <<"CatchClause">>},
                               {"param", Param},
                               {"guard", Guard},
                               {"body", Body}]).

%% Generates a while statement
-spec whileStatement(expression_node(),
                     statement_node()) -> while_statement_node().
whileStatement(Expression, Body) ->
    %?spec([{Expression, expression}, {Body, statement}]),
    updateRecord(statement(), [{"type", <<"WhileStatement">>},
                               {"test", Expression},
                               {"body", Body}]).

%% Generates a for statement
-spec forStatement(expression_node() | variable_declaration_node() | 'null',
                   expression_node() | 'null',
                   expression_node(),
                   statement_node()) -> for_statement_node().
forStatement(Init, Update, Expression, Body) ->
    %?spec([{Init, [expression, variableDeclaration, null]},
    %       {Update, [expression, null]},
    %       {Expression, expression},
    %       {Body, statement}]),
    updateRecord(whileStatement(Expression, Body), [{"type", <<"ForStatement">>},
                                                    {"init", Init},
                                                    {"update", Update}]).

%% Generates a forIn statement
-spec forInStatement(variable_declaration_node() | expression_node(),
                     expression_node(),
                     statement_node(),
                     boolean()) -> for_in_statement_node().
forInStatement(Left, Right, Body, Each) ->
    %?spec([{Left, [variableDeclaration, expression]},
    %       {Right, expression},
    %       {Body, statement},
    %       {Each, boolean}]),
    updateRecord(statement(), [{"type", <<"ForInStatement">>},
                               {"left", Left},
                               {"right", Right},
                               {"each", Each},
                               {"body", Body}]).

%% Generates a forOf statement
-spec forOfStatement(variable_declaration_node() | expression_node(),
                     expression_node(),
                     statement_node()) -> for_of_statement_node().
forOfStatement(Left, Right, Body) ->
    %?spec([{Left, [variableDeclaration, expression]}, {Right, expression}, {Body, statement}]),
    updateRecord(statement(), [{"type", <<"ForOfStatement">>},
                               {"left", Left},
                               {"right", Right},
                               {"body", Body}]).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - SHORTHANDS FUNCTIONS ----------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Throw an error string
-spec error(iolist(),
            iolist(),
            expression_node()) -> throw_statement_node().
error(Type, Description, Message) ->
    throwStatement(
        binaryExpression(<<"+">>,
            literal(iolist_to_binary(["** ", Type, ": ", Description])),
            Message
        )
    ).

%% Use strict directive
-spec useStrict() -> expression_statement_node().
useStrict() ->
    expressionStatement(literal(<<"use_strict">>)).

%% Variable declarator variant shorthand
-spec constDeclaration(es_identifier(),
                       expression_node()) -> variable_declaration_node().
constDeclaration(Identifier, Init) ->
    variableDeclaration([variableDeclarator(identifier(Identifier), Init)], <<"const">>).

-spec varDeclaration(es_identifier(),
                     expression_node()) -> variable_declaration_node().
varDeclaration(Identifier, Init) ->
    variableDeclaration([variableDeclarator(identifier(Identifier), Init)], <<"var">>).

-spec letDeclaration(es_identifier(),
                     expression_node()) -> variable_declaration_node().
letDeclaration(Identifier, Init) ->
    variableDeclaration([variableDeclarator(identifier(Identifier), Init)], <<"let">>).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - META FUNCTIONS ----------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

-ifdef(TEST).
-spec eunit() -> ok.
eunit() ->
    eunit:test({inparallel, ?MODULE}),
    init:stop().
-endif.