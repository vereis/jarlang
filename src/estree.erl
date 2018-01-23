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
        source_location/3,
        add_location_data/4,

        declaration/0,
        expression/0,
        statement/0,

        identifier/1,
        literal/1,
        program/1,

        spread_element/1,

        variable_declaration/2,
        variable_declarator/2,
        function_declaration/3,

        this_expression/0,
        array_expression/1,
        object_expression/1,
        property/2,
        function_expression/4,
        sequence_expression/1,
        unary_expression/3,
        assignment_expression/3,
        binary_expression/3,
        update_expression/3,
        logical_expression/3,
        conditional_expression/3,
        new_expression/2,
        call_expression/2,
        member_expression/3,
        arrow_expression/6,
        yield_expression/1,
        generator_expression/3,
        comprehension_expression/3,
        comprehension_block/3,
        comprehension_if/1,

        empty_statement/0,
        expression_statement/1,
        block_statement/1,
        if_statement/3,
        labeled_statement/2,
        break_statement/1,
        continue_statement/1,
        with_statement/2,
        switch_statement/3,
        switch_case/2,
        return_statement/1,
        throw_statement/1,
        try_statement/4,
        catch_clause/3,
        while_statement/2,
        for_statement/4,
        for_in_statement/4,
        for_of_statement/3,
        do_while_statement/2,
        debugger_statement/0,

        error/3,
        use_strict/0,
        const_declaration/2,
        var_declaration/2,
        let_declaration/2
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
                       | binary()
                       | string()
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
                              | member_expression_node()
                              | generator_expression_node()
                              | comprehension_expression_node()
                              | yield_expression_node()
                              | arrow_expression_node()
                              | spread_element_node().

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
                              | for_of_statement_node()
                              | do_while_statement_node()
                              | debugger_statement_node().

%% Declarations
-type variable_declaration_node() :: {'__estree_node', 'VariableDeclaration', PropertyFields::es_node_fields()}.

-type variable_declarator_node()  :: {'__estree_node', 'VariableDeclarator', PropertyFields::es_node_fields()}.

-type function_declaration_node() :: {'__estree_node', 'FunctionDeclaration', PropertyFields::es_node_fields()}.

%% Expressions
-type this_expression_node()          :: {'__estree_node', 'ThisExpression', PropertyFields::es_node_fields()}.

-type array_expression_node()         :: {'__estree_node', 'ArrayExpression', PropertyFields::es_node_fields()}.

-type object_expression_node()        :: {'__estree_node', 'ObjectExpression', PropertyFields::es_node_fields()}.

-type object_property_node()          :: {'__estree_node', 'Property', PropertyFields::es_node_fields()}.

-type function_expression_node()      :: {'__estree_node', 'FunctionExpression', PropertyFields::es_node_fields()}.

-type sequence_expression_node()      :: {'__estree_node', 'SequenceExpression', PropertyFields::es_node_fields()}.

-type unary_expression_node()         :: {'__estree_node', 'UnaryExpression', PropertyFields::es_node_fields()}.

-type assignment_expression_node()    :: {'__estree_node', 'AssignmentExpression', PropertyFields::es_node_fields()}.

-type binary_expression_node()        :: {'__estree_node', 'BinaryExpression', PropertyFields::es_node_fields()}.

-type update_expression_node()        :: {'__estree_node', 'UpdateExpression', PropertyFields::es_node_fields()}.

-type logical_expression_node()       :: {'__estree_node', 'LogicalExpression', PropertyFields::es_node_fields()}.

-type conditional_expression_node()   :: {'__estree_node', 'ConditionalExpression', PropertyFields::es_node_fields()}.

-type new_expression_node()           :: {'__estree_node', 'NewExpression', PropertyFields::es_node_fields()}.

-type call_expression_node()          :: {'__estree_node', 'CallExpression', PropertyFields::es_node_fields()}.

-type member_expression_node()        :: {'__estree_node', 'MemberExpression', PropertyFields::es_node_fields()}.

-type generator_expression_node()     :: {'__estree_node', 'GeneratorExpression', PropertyFields::es_node_fields()}.

-type comprehension_expression_node() :: {'__estree_node', 'ComprehensionExpression', es_node_fields()}.

-type comprehension_if_node()         :: {'__estree_node', 'ComprehensionIf', PropertyFields::es_node_fields()}.

-type comprehension_block_node()      :: {'__estree_node', 'ComprehensionBlock', PropertyFields::es_node_fields()}.

-type yield_expression_node()         :: {'__estree_node', 'YieldExpression', PropertyFields::es_node_fields()}.

-type arrow_expression_node()         :: {'__estree_node', 'ArrowExpression', PropertyFields::es_node_fields()}.

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

-type do_while_statement_node()       :: {'__estree_node', 'DoWhileStatement', PropertyFields::es_node_fields()}.

-type debugger_statement_node()    :: {'__estree_node', 'DebuggerStatement', PropertyFields::es_node_fields()}.

%% Export all node types for external use
-export_type([
    es_literal/0,
    es_identifier/0,
    es_node/0,
    es_node_type/0,
    es_node_field/0,
    es_node_fields/0,
    tba_node/0,
    identifier_node/0,
    source_location_node/0,
    position_node/0,
    literal_node/0,
    program_node/0,
    spread_element_node/0,
    declaration_node/0,
    expression_node/0,
    statement_node/0,
    variable_declaration_node/0,
    variable_declarator_node/0,
    function_declaration_node/0,
    this_expression_node/0,
    array_expression_node/0,
    object_expression_node/0,
    object_property_node/0,
    function_expression_node/0,
    sequence_expression_node/0,
    unary_expression_node/0,
    assignment_expression_node/0,
    binary_expression_node/0,
    update_expression_node/0,
    logical_expression_node/0,
    conditional_expression_node/0,
    new_expression_node/0,
    call_expression_node/0,
    member_expression_node/0,
    generator_expression_node/0,
    comprehension_expression_node/0,
    comprehension_if_node/0,
    comprehension_block_node/0,
    yield_expression_node/0,
    arrow_expression_node/0,
    empty_statement_node/0,
    expression_statement_node/0,
    block_statement_node/0,
    if_statement_node/0,
    labeled_statement_node/0,
    break_statement_node/0,
    continue_statement_node/0,
    with_statement_node/0,
    switch_statement_node/0,
    switch_case_statement_node/0,
    return_statement_node/0,
    throw_statement_node/0,
    try_statement_node/0,
    catch_clause_node/0,
    while_statement_node/0,
    for_statement_node/0,
    for_in_statement_node/0,
    for_of_statement_node/0,
    do_while_statement_node/0,
    debugger_statement_node/0
]).





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
    update_record(NewNode, Fs);
node(T, Fs) when is_list(T) ->
    node(list_to_atom(T), Fs).

%% Updates a given node with the given NodeFields
-spec update_record(es_node(),
                    es_node_fields()) -> es_node().
update_record({_, _, NodeFields}, NewFields) ->
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
-spec add_location_data(es_node(),
                        pos_integer(),
                        non_neg_integer(),
                        non_neg_integer()) -> es_node().
add_location_data(Node, LineNumber, ColStart, ColEnd) ->
    update_record(Node, [{"loc", source_location(LineNumber, ColStart, ColEnd)}]).

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
-spec source_location(pos_integer(),
                      non_neg_integer(),
                      non_neg_integer()) -> source_location_node().
source_location(LineNumber, ColStart, ColEnd) ->
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
-spec spread_element(identifier_node() | array_expression_node()) -> spread_element_node().
spread_element(Argument) ->
    %?spec([{Argument, [identifier, arrayExpression]}]),
    update_record(node(), [{"type", <<"SpreadElement">>},
                           {"argument", Argument}]).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - DECLARATION NODES -------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Generates a variable declaration
-spec variable_declaration([variable_declarator_node()],
                            es_literal()) -> variable_declaration_node().
variable_declaration(Declarations, Kind)  ->
    %?spec([{Declarations, list_of_variableDeclarator}, {Kind, [variableType]}]),
    update_record(declaration(), [{"type", <<"VariableDeclaration">>},
                                  {"declarations", Declarations},
                                  {"kind", Kind}]).

%% Generates a variable declarator
-spec variable_declarator(identifier_node(),
                          expression_node()) -> variable_declarator_node().
variable_declarator(Identifier, Init) ->
    %?spec([{Identifier, identifier}, {Init, expression}]),
    node("VariableDeclarator", [{"id", Identifier},
                                {"init", Init}]).

%% Generate a function declaration
-spec function_declaration(identifier_node(),
                           [identifier_node()],
                           block_statement_node()) -> function_declaration_node().
function_declaration(Identifier, Params, Body) ->
    %?spec([{Identifier, identifier}, {Params, list_of_identifier}, {Body, [expression, blockStatement]}]),
    update_record(declaration(), [{"type", <<"FunctionDeclaration">>},
                                  {"id", Identifier},
                                  {"params", Params},
                                  {"body", Body}]).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - EXPRESSION NODES --------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Generate a This expression
-spec this_expression() -> this_expression_node().
this_expression() ->
    update_record(expression(), [{"type", <<"ThisExpression">>}]).

%% Generate an Array expression
-spec array_expression([expression_node()]) -> array_expression_node().
array_expression(Elements) ->
    %?spec([{Elements, list_of_expression}]),
    update_record(expression(), [{"type", <<"ArrayExpression">>},
                                 {"elements", Elements}]).

%% Generate an Object expression
-spec object_expression([object_property_node()]) -> object_expression_node().
object_expression(Properties) ->
    %?spec([{Properties, list_of_property}]),
    update_record(expression(), [{"type", <<"ObjectExpression">>},
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
-spec function_expression(identifier_node() | 'null',
                          [identifier_node()],
                          block_statement_node(),
                          boolean()) -> function_expression_node().
function_expression(Identifier, Params, Body, Expression) ->
    %?spec([{Identifier, [identifier, null]},
    %       {Params, list_of_identifier},
    %       {Body, blockStatement},
    %       {Expression, boolean}]),
    update_record(expression(), [{"type", <<"FunctionExpression">>},
                                 {"id", Identifier},
                                 {"params", Params},
                                 {"body", Body},
                                 {"expression", Expression}]).

%% Generates an arrow expression
-spec arrow_expression([identifier_node()],
                       [expression_node()],
                       identifier_node() | 'null',
                       block_statement_node() | expression_node(),
                       boolean(),
                       boolean()) -> arrow_expression_node().
arrow_expression(Params, Defaults, Rest, Body, IsGenerator, IsExpression) ->
    update_record(expression(), [{"type", <<"ArrowExpression">>},
                                 {"params", Params},
                                 {"defaults", Defaults},
                                 {"rest", Rest},
                                 {"body", Body},
                                 {"generator", IsGenerator},
                                 {"expression", IsExpression}]).

%% Generate a sequence expression
-spec sequence_expression([expression_node()]) -> sequence_expression_node().
sequence_expression(Expressions) ->
    %?spec([{Expressions, list_of_expression}]),
    update_record(expression(), [{"type", <<"SequenceExpression">>},
                                 {"expressions", Expressions}]).

%% Generate a unary operation expression
-spec unary_expression(any(),
                       boolean(),
                       expression_node()) -> unary_expression_node().
unary_expression(Operator, Prefix, Argument) when ?IS_UNARY_OPERATOR(Operator)  ->
    %?spec([{Operator, anything},
    %       {Prefix, boolean},
    %       {Argument, expression}]),
    update_record(expression(), [{"type", <<"UnaryExpression">>},
                                 {"operator", Operator},
                                 {"prefix", Prefix},
                                 {"argument", Argument}]).

%% Generate an assignment expression
-spec assignment_expression(any(),
                            expression_node(),
                            expression_node()) -> assignment_expression_node().
assignment_expression(Operator, Left, Right) ->
    %?spec([{Operator, anything},
    %       {Left, expression},
    %       {Right, expression}]),
    update_record(expression(), [{"type", <<"AssignmentExpression">>},
                                 {"operator", Operator},
                                 {"left", Left},
                                 {"right", Right}]).

%% Generate a binary operation expression
-spec binary_expression(any(),
                        expression_node(),
                        expression_node()) -> binary_expression_node().
binary_expression(Operator, Left, Right) when ?IS_BINARY_OPERATOR(Operator) ->
    %?spec([{Operator, anything},
    %       {Left, expression},
    %       {Right, expression}]),
    update_record(expression(), [{"type", <<"BinaryExpression">>},
                                 {"operator", Operator},
                                 {"left", Left},
                                 {"right", Right}]).

%% Generate an update (increment or decrement) operator expression
-spec update_expression(any(),
                        expression_node(),
                        boolean()) -> update_expression_node().
update_expression(Operator, Argument, Prefix) when ?IS_UPDATE_OPERATOR(Operator) ->
    %?spec([{Operator, anything},
    %       {Argument, expression},
    %       {Prefix, boolean}]),
    update_record(expression(), [{"type", <<"UpdateExpression">>},
                                 {"operator", Operator},
                                 {"argument", Argument},
                                 {"prefix", Prefix}]).

%% Generate a logical operator expression
-spec logical_expression(any(),
                         expression_node(),
                         expression_node()) -> logical_expression_node().
logical_expression(Operator, Left, Right) when ?IS_LOGICAL_OPERATOR(Operator) ->
    %?spec([{Operator, anything},
    %       {Left, expression},
    %       {Right, expression}]),
    update_record(expression(), [{"type", <<"LogicalExpression">>},
                                 {"operator", Operator},
                                 {"left", Left},
                                 {"right", Right}]).

%% Generate a conditional expression
-spec conditional_expression(expression_node(),
                             expression_node(),
                             expression_node()) -> conditional_expression_node().
conditional_expression(Test, Alternate, Consequent) ->
    %?spec([{Test, expression}, {Alternate, expression}, {Consequent, expression}]),
    update_record(expression(), [{"type", <<"ConditionalExpression">>},
                                 {"test", Test},
                                 {"alternate", Alternate},
                                 {"consequent", Consequent}]).

%% Generate a new expression
-spec new_expression(expression_node(),
                     [expression_node()]) -> new_expression_node().
new_expression(Callee, Arguments) ->
    %?spec([{Callee, expression}, {Arguments, list_of_expression}]),
    update_record(expression(), [{"type", <<"NewExpression">>},
                                 {"callee", Callee},
                                 {"arguments", Arguments}]).

%% Generate a call expression
-spec call_expression(expression_node(),
                      [expression_node()]) -> call_expression_node().
call_expression(Callee, Arguments) ->
    %?spec([{Callee, expression}, {Arguments, list_of_expression}]),
    update_record(expression(), [{"type", <<"CallExpression">>},
                                 {"callee", Callee},
                                 {"arguments", Arguments}]).

%% Generate a member expression
-spec member_expression(expression_node(),
                        identifier_node() | expression_node(),
                        boolean()) -> member_expression_node().
member_expression(Object, Property, Computed) ->
    %?spec([{Object, expression}, {Property, [identifier, expression]}, {Computed, boolean}]),
    update_record(expression(), [{"type", <<"MemberExpression">>},
                                 {"object", Object},
                                 {"property", Property},
                                 {"computed", Computed}]).

%% Generate a yield expression
-spec yield_expression(expression_node() | 'null') -> yield_expression_node().
yield_expression(Argument) ->
    update_record(expression(), [{"type", <<"YieldExpression">>},
                                 {"argument", Argument}]).

%% Generate an array comprehension expression
-spec comprehension_expression(expression_node(),
                               [comprehension_block_node() | comprehension_if_node()],
                               expression_node() | 'null') -> comprehension_expression_node().
comprehension_expression(Body, Blocks, Filter) ->
    update_record(expression(), [{"type", <<"ComprehensionExpression">>},
                                 {"body", Body},
                                 {"blocks", Blocks},
                                 {"filter", Filter}]).

%% Generates a comprehension block node
-spec comprehension_block(any(),
                          expression_node(),
                          boolean()) -> comprehension_block_node().
comprehension_block(Left, Right, Each) ->
    update_record(expression(), [{"type", <<"ComprehensionBlock">>},
                                 {"left", Left},
                                 {"right", Right},
                                 {"each", Each}]).

%% Generates a comprehension if node
-spec comprehension_if(expression_node()) -> comprehension_if_node().
comprehension_if(Test) ->
    update_record(expression(), [{"type", <<"ComprehensionIf">>},
                                 {"test", Test}]).

%% Generates a generator expression
-spec generator_expression(expression_node(),
                           [comprehension_block_node() | comprehension_if_node()],
                           expression_node() | 'null') -> generator_expression_node().
generator_expression(Body, Blocks, Filter) ->
    update_record(expression(), [{"type", <<"GeneratorExpression">>},
                                 {"body", Body},
                                 {"blocks", Blocks},
                                 {"filter", Filter}]).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - STATEMENT NODES ---------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Generates an empty statement node - i.e. an empty semicolon
-spec empty_statement() -> empty_statement_node().
empty_statement() ->
    update_record(statement(), [{"type", <<"EmptyStatement">>}]).

%% Generates an Expression Statement
-spec expression_statement(expression_node()) -> expression_statement_node().
expression_statement(Expression) ->
    update_record(statement(), [{"type", <<"ExpressionStatement">>},
                                {"expression", Expression}]).

%% Generates a block statement
-spec block_statement([statement_node()]) -> block_statement_node().
block_statement(Body) ->
    %?spec([{Body, list_of_statement}]),
    update_record(statement(), [{"type", <<"BlockStatement">>},
                                {"body", Body}]).

%% Generates an if statement
-spec if_statement(expression_node(),
                   statement_node(),
                   statement_node() | 'null') -> if_statement_node().
if_statement(Test, Consequent, Alternate) ->
    %?spec([{Test, expression}, {Consequent, statement}, {Alternate, [statement, null]}]),
    update_record(statement(), [{"type", <<"IfStatement">>},
                                {"test", Test},
                                {"consequent", Consequent},
                                {"alternate", Alternate}]).

%% Generates a Labeled statement (prefixed with break/continue)
-spec labeled_statement(identifier_node(),
                        statement_node()) -> labeled_statement_node().
labeled_statement(Identifier, Body) ->
    %?spec([{Identifier, identifier}, {Body, statement}]),
    update_record(statement(), [{"type", <<"LabeledStatement">>},
                                {"label", Identifier},
                                {"body", Body}]).

%% Generates a break statement
-spec break_statement(identifier_node() | 'null') -> break_statement_node().
break_statement(Identifier) ->
    %?spec([{Identifier, [null, identifier]}]),
    update_record(statement(), [{"type", <<"BreakStatement">>},
                                {"label", Identifier}]).

%% Generates a continue statement
-spec continue_statement(identifier_node() | 'null') -> continue_statement_node().
continue_statement(Identifier) ->
    %?spec([{Identifier, [null, identifier]}]),
    update_record(statement(), [{"type", <<"ContinueStatement">>},
                                {"label", Identifier}]).

%% Generates a with statement
-spec with_statement(expression_node(),
                     block_statement_node()) -> with_statement_node().
with_statement(Expression, BlockStatement) ->
    %?spec([{Expression, expression}, {BlockStatement, blockStatement}]),
    update_record(statement(), [{"type", <<"WithStatement">>},
                                {"object", Expression},
                                {"body", BlockStatement}]).

%% Generates a switch statement
-spec switch_statement(expression_node(),
                       [switch_case_statement_node()],
                       boolean()) -> switch_statement_node().
switch_statement(Expression, Cases, HasLexScope) ->
    %?spec([{Expression, expression}, {Cases, list_of_switchCase}, {HasLexScope, boolean}]),
    update_record(statement(), [{"type", <<"SwitchStatement">>},
                                {"discriminant", Expression},
                                {"cases", Cases},
                                {"lexical", HasLexScope}]).

%% Generates a case for use in a switch statement
-spec switch_case(expression_node(),
                  [statement_node()]) -> switch_case_statement_node().
switch_case(Test, Consequent) ->
    %?spec([{Test, expression}, {Consequent, list_of_statement}]),
    update_record(statement(), [{"type", <<"SwitchCase">>},
                                {"test", Test},
                                {"consequent", Consequent}]).

%% Generates a return statement
-spec return_statement(expression_node() | 'null') -> return_statement_node().
return_statement(Expression) ->
    %?spec([{Expression, [null, expression]}]),
    update_record(statement(), [{"type", <<"ReturnStatement">>},
                                {"argument", Expression}]).

%% Generates a throw statement
-spec throw_statement(expression_node() | 'null') -> throw_statement_node().
throw_statement(Expression) ->
    %?spec([{Expression, [null, expression]}]),
    update_record(statement(), [{"type", <<"ThrowStatement">>},
                                {"argument", Expression}]).

%% Generates a try statement
-spec try_statement(block_statement_node(),
                    catch_clause_node(),
                    [catch_clause_node()],
                    block_statement_node()) -> try_statement_node().
try_statement(BlockStatement, Handler, GuardedHandler, Finalizer) ->
    %?spec([{BlockStatement, blockStatement},
    %       {Handler, catchClause},
    %       {GuardedHandler, list_of_catchClause},
    %       {Finalizer, blockStatement}]),
    update_record(statement(), [{"type", <<"TryStatement">>},
                                {"block", BlockStatement},
                                {"handler", Handler},
                                {"guardedHandler", GuardedHandler},
                                {"finalizer", Finalizer}]).

%% Generates a catch clause for use in a try statement
-spec catch_clause(identifier_node(),
                   expression_node(),
                   block_statement_node()) -> catch_clause_node().
catch_clause(Param, Guard, Body) ->
    %?spec([{Param, identifier}, {Guard, expression}, {Body, blockStatement}]),
    update_record(statement(), [{"type", <<"CatchClause">>},
                                {"param", Param},
                                {"guard", Guard},
                                {"body", Body}]).

%% Generates a while statement
-spec while_statement(expression_node(),
                      statement_node()) -> while_statement_node().
while_statement(Expression, Body) ->
    %?spec([{Expression, expression}, {Body, statement}]),
    update_record(statement(), [{"type", <<"WhileStatement">>},
                                {"test", Expression},
                                {"body", Body}]).

%% Generates a doWhile statement
-spec do_while_statement(expression_node(),
                         statement_node()) -> do_while_statement_node().
do_while_statement(Expression, Body) ->
    update_record(statement(), [{"type", <<"DoWhileStatement">>},
                                {"test", Expression},
                                {"body", Body}]).

%% Generates a for statement
-spec for_statement(expression_node() | variable_declaration_node() | 'null',
                    expression_node() | 'null',
                    expression_node(),
                    statement_node()) -> for_statement_node().
for_statement(Init, Update, Expression, Body) ->
    %?spec([{Init, [expression, variableDeclaration, null]},
    %       {Update, [expression, null]},
    %       {Expression, expression},
    %       {Body, statement}]),
    update_record(while_statement(Expression, Body), [{"type", <<"ForStatement">>},
                                                      {"init", Init},
                                                      {"update", Update}]).

%% Generates a forIn statement
-spec for_in_statement(variable_declaration_node() | expression_node(),
                       expression_node(),
                       statement_node(),
                       boolean()) -> for_in_statement_node().
for_in_statement(Left, Right, Body, Each) ->
    %?spec([{Left, [variableDeclaration, expression]},
    %       {Right, expression},
    %       {Body, statement},
    %       {Each, boolean}]),
    update_record(statement(), [{"type", <<"ForInStatement">>},
                                {"left", Left},
                                {"right", Right},
                                {"each", Each},
                                {"body", Body}]).

%% Generates a forOf statement
-spec for_of_statement(variable_declaration_node() | expression_node(),
                       expression_node(),
                       statement_node()) -> for_of_statement_node().
for_of_statement(Left, Right, Body) ->
    %?spec([{Left, [variableDeclaration, expression]}, {Right, expression}, {Body, statement}]),
    update_record(statement(), [{"type", <<"ForOfStatement">>},
                                {"left", Left},
                                {"right", Right},
                                {"body", Body}]).

%% Generates a debugger statement
-spec debugger_statement() -> debugger_statement_node().
debugger_statement() ->
    update_record(statement(), [{"type", <<"DebuggerStatement">>}]).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - SHORTHANDS FUNCTIONS ----------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Throw an error string
-spec error(iolist(),
            iolist(),
            expression_node()) -> throw_statement_node().
error(Type, Description, Message) ->
    throw_statement(
        binary_expression(<<"+">>,
            literal(iolist_to_binary(["** ", Type, ": ", Description])),
            Message
        )
    ).

%% Use strict directive
-spec use_strict() -> expression_statement_node().
use_strict() ->
    expression_statement(literal(<<"use_strict">>)).

%% Variable declarator variant shorthand
-spec const_declaration(es_identifier(),
                        expression_node()) -> variable_declaration_node().
const_declaration(Identifier, Init) ->
    variable_declaration([variable_declarator(identifier(Identifier), Init)], <<"const">>).

-spec var_declaration(es_identifier(),
                      expression_node()) -> variable_declaration_node().
var_declaration(Identifier, Init) ->
    variable_declaration([variable_declarator(identifier(Identifier), Init)], <<"var">>).

-spec let_declaration(es_identifier(),
                      expression_node()) -> variable_declaration_node().
let_declaration(Identifier, Init) ->
    variable_declaration([variable_declarator(identifier(Identifier), Init)], <<"let">>).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - META FUNCTIONS ----------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

-ifdef(TEST).
-spec eunit() -> ok.
eunit() ->
    eunit:test({inparallel, ?MODULE}),
    init:stop().
-endif.
