%%% Module Description:
%%% Recursively descends into a core erlang AST and generates an ESTREE equivalent
-module(asttrans).
-author(["Andrew Johnson", "Chris Bailey", "Nick Laine"]).

-define(VERSION, "2.1.0").

-vsn(?VERSION).

%%% Export all functions if we compiled with erlc -dTEST or c(?MODULE, {d, 'TEST'}).
%%% This is so that we can run external eunit tests
-ifdef(TEST).
    -compile(export_all).
-else.
    -export([erast_to_esast/2]).
-endif.





%%% ---------------------------------------------------------------------------------------------%%%
%%% - MODULE ENTRYPOINTS ------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Alternative entrypoint to parse_module/1
erast_to_esast(AST, ConcMode) ->
    parse_module(AST, ConcMode).

%% Begins parses ast expecting a c_module atom to indicate that the ast we're parsing
%% is indeed a core_erlang ast.
%% Processes information we need such as ModuleName and continues to parse functions and
%% module export information.
parse_module({c_module, _A, {_, _, ModuleName}, Exports, _Attributes, Functions}, ConcMode) ->
    FormattedFunctions = parse_functions(Functions, ConcMode),
    FormattedExports = lists:map(
        fun({N, A}) ->{atom_to_list(N), A} end,
        tuple_list_get_vars_3(Exports)
    ),
    esast:c_module(atom_to_list(ModuleName), FormattedExports, FormattedFunctions);
parse_module(T, _ConcMode) ->
    io:format("Unrecognised Token in module section: ~p", [T]).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Concurrently parse all functions in the module --------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
parse_functions(Functions, multi_threaded) ->
    Self = self(),
    Pids = lists:map(
        fun(X) ->
            spawn_link(fun() -> Self ! {self(), parse_function(X)} end)
        end, Functions
    ),
    [
        receive
            {Pid, TranspiledFunction} ->
                TranspiledFunction
        end
        ||
        Pid <- Pids
    ];
parse_functions(Functions, single_threaded) ->
    [parse_function(Function) || Function <- Functions].


%%% ---------------------------------------------------------------------------------------------%%%
%%% - Parse a funcion node ----------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
%% Compiler generated functions are currently generated as an empty function.
parse_function({{_, _, {FunctionName, Arity}}, {_c_fun, [compiler_generated], _, _}}) ->
    {atom_to_list(FunctionName) ++ "/" ++ integer_to_list(Arity), function_wrap([], [])};
parse_function({{_, _, {FunctionName, Arity}}, {_c_fun, _, ParamNames, Body}}) ->
    {
        atom_to_list(FunctionName) ++ "/" ++ integer_to_list(Arity), 
        function_wrap(
            lists:map(fun(N) -> parse_var(noreturn, [], N) end, ParamNames),
            parse_node(return, tuple_list_get_vars_3(ParamNames), Body)
        )
    }.



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Select the appropriate node parsing funciton ----------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
%% Dividing the pattern matching in this way makes the stack traces easier to read
parse_node(ReturnAtom, Params, N) ->
    case tuple_getVar_1(N) of
        c_call    -> parse_call(ReturnAtom, Params, N);
        c_var     -> parse_var(ReturnAtom, Params, N);
        c_seq     -> parse_seq(ReturnAtom, Params, N);
        c_let     -> parse_let(ReturnAtom, Params, N);
        c_apply   -> parse_apply(ReturnAtom, Params, N);
        c_literal -> parse_literal(ReturnAtom, Params, N);
        c_tuple   -> parse_tuple(ReturnAtom, Params, N);
        c_cons    -> parse_cons(ReturnAtom, Params, N);
        c_try     -> parse_try(ReturnAtom, Params, N);
        c_primop  -> parse_primop(ReturnAtom, Params, N);
        c_letrec  -> parse_letrec(ReturnAtom, Params, N);
        c_receive -> parse_receive(ReturnAtom, Params, N);
        c_case    -> parse_case(ReturnAtom, Params, N)
    end.



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Parse a function call node ----------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
parse_call(return, Params, {c_call, A, {B, C, Module}, {D, E, FunctionName}, Parameters}) ->
    estree:return_statement(
        parse_call(noreturn, Params, {c_call, A, {B, C, Module}, {D, E, FunctionName}, Parameters})
    );
parse_call(noreturn, Params, {c_call, _, {_, _, Module}, {_, _, FunctionName}, Parameters}) ->
    estree:call_expression(
        estree:call_expression(
            estree:member_expression(
                estree:member_expression(
                    estree:identifier(atom_to_binary(Module, utf8)),
                    estree:literal(atom_to_binary(FunctionName, utf8)),
                    true),
                estree:identifier(<<"bind">>),
                false
            ),
            [estree:this_expression()]
        ),
        lists:map(fun(T) -> parse_node(noreturn, Parameters, T) end, Parameters)
     ).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Parse the use of a variable ---------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
parse_var(return, Params, {c_var, A, Var}) ->
    estree:return_statement(parse_var(noreturn, Params, {c_var, A, Var}));
%added for support of functions by name
parse_var(noreturn, Params, {c_var, _, {Name, Arity}}) ->
    estree:identifier(atom_to_list(Name) ++ "/" ++ integer_to_list(Arity));
parse_var(noreturn, Params, {c_var, _, Var}) ->
    estree:identifier(atom_to_binary(Var, utf8)).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Parse a sequence of nodes -----------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
%% If the first node is a receive node the second node must be passed as a parameter to be called
%% inside the receive wrapping.
parse_seq(ReturnAtom, Params, {c_seq, _, A={c_receive,_,_,_,_}, B}) ->
        parse_node(noreturn,
            {next,{ReturnAtom, Params, B}}, A);
parse_seq(ReturnAtom, Params, {c_seq, _, A, B}) ->
    assemble_sequence(
        parse_node(noreturn, Params, A),
        parse_node(ReturnAtom, Params, B)).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Parse the assinment of a variable for immediate use ---------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
%% A let statement is the core erlang representation of implicit variable declaration
%% Often the result of some function as an argument of another function.
parse_let(ReturnAtom, Params, {c_let, _, [{_, _, Variable}], Value, UsedBy}) ->
    assemble_sequence(
        estree:variable_declaration(
            [estree:variable_declarator(
                estree:identifier(atom_to_binary(Variable, utf8)),
                parse_node(noreturn, Params, Value))
            ],
            <<"let">>
        ),
        parse_node(ReturnAtom, Params, UsedBy)).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Parse the call of a local function --------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
parse_apply(ReturnAtom, Params, {c_apply, _, {_, _, {FunctionName, Arity}}, Parameters}) ->
    parse_call(ReturnAtom, Params, {c_call, [], {a, a, functions},
        {a, a, list_to_atom(atom_to_list(FunctionName) ++ "/" ++ integer_to_list(Arity))},
        Parameters}
    );
parse_apply(ReturnAtom, Params, {c_apply, _, {_, _, FunctionName}, Parameters}) ->
    parse_call(ReturnAtom, Params,
        {c_call, [], {a, a, functions}, {a, a, FunctionName}, Parameters}
    ).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Parse the use of a literal ----------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
%% Note that a list of literals is also a literal,
%% but that the internal values are represented as actual literals, not literal nodes.
parse_literal(return, Params, {c_literal, _, Value}) ->
    estree:return_statement(parse_literal(noreturn, Params, {c_literal, [], Value}));
parse_literal(noreturn, Params, {c_literal, _, Value}) when is_integer(Value) ->
    estree:new_expression(estree:identifier(<<"Int">>), [estree:literal(Value)]);
parse_literal(noreturn, Params, {c_literal, _, Value}) when is_float(Value) ->
    estree:new_expression(estree:identifier(<<"Float">>), [estree:literal(Value)]);
parse_literal(noreturn, Params, {c_literal, _, Value}) when is_atom(Value) ->
    estree:new_expression(
        estree:identifier(<<"Atom">>), [estree:literal(atom_to_binary(Value, utf8))]
    );

parse_literal(noreturn, Params, {c_literal, _, Value}) when is_list(Value) ->
    estree:new_expression(estree:identifier(<<"List">>), lists:map(fun(Elem) ->
        parse_literal(noreturn, Params, {c_literal, [], Elem})
    end, Value));
parse_literal(noreturn, Params, {c_literal, _, Value}) when is_tuple(Value) ->
    estree:new_expression(estree:identifier(<<"Tuple">>),
        case tup_to_list(Value) of
            [] -> [];
            Array -> [parse_literal(return, Params, {c_literal, [], A}) || A <- Array]
        end
    ).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Parse the definition of a tuple that doesn't contain only literals ------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
parse_tuple(return, Params, A={c_tuple, _, Values}) ->
    estree:return_statement(parse_tuple(noreturn, Params, A));
parse_tuple(noreturn, Params, {c_tuple, _, Values}) ->
    estree:new_expression(
        estree:identifier(<<"Tuple">>), [parse_node(noreturn, Params, Value) || Value <- Values]
    ).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Parse the definition of a list that doesn't contain only literals -------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
%% This function parses the start of the list
parse_cons(return,Params,{c_cons,_,A,B})->
    estree:return_statement(parse_cons(noreturn,Params,{c_cons,[],A,B}));
parse_cons(noreturn,Params,{c_cons,_,A,B})->
    {Values,End} = parse_cons_chain(noreturn,Params,{c_cons,[],A,B}),
    NewList = estree:new_expression(estree:identifier(<<"List">>),Values),
    estree:call_expression(
        estree:member_expression(NewList,estree:identifier(<<"cons">>),false),
        [End]
    ).

%% This function parses the rest of the list
parse_cons_chain(noreturn,Params,{c_cons,[],A,B={c_cons,_,C,D}})->
    {Values,End} = parse_cons_chain(noreturn,[],B),
    {[parse_node(noreturn,Params,A)|Values],End};
parse_cons_chain(noreturn,Params,{c_cons,[],A,B})->
    {[parse_node(noreturn,Params,A)],parse_node(noreturn,Params,B)}.



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Naively parse a try wrapper, assuming it has no actual purpose ----------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
%% c_try nodes are not associated with actual try blocks in erlang, but instead generated by the 
%% compiler around some functions when used in a guard.
parse_try(return, Params, {c_try, _, Elem, _, _, _, _}) ->
    estree:return_statement(parse_try(noreturn, Params, {c_try, [], Elem, a, a, a, a}));
parse_try(noreturn, Params, {c_try, _, Elem, _, _, _, _}) ->
    estree:call_expression(
        function_wrap(
            [],
            parse_node(return, Params, Elem)
        ),
    []).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Naively parse the creation of an error message --------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
%% c_primop has so far only been observed as an error message
parse_primop(return, Params, {c_primop, _, {_, _, Type}, _Details}) ->
    estree:error(
        atom_to_list(Type),
        "TODO Errors dont parse nicely\\n", estree:literal(<<"Message">>)
    ).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Parse a list comprehension ----------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
%% TODO: List comprehensions are always created with the same ID, multiple concurrent list
%% comprehensions will behave incorrectly until they are defined in an appropriate scope
parse_letrec(ReturnAtom, Params, {c_letrec, _, [Func], Apply}) ->
    {Id, F} = parse_function(Func),
    assemble_sequence(
        estree:assignment_expression(
            <<"=">>,
            estree:member_expression(
                estree:identifier(<<"functions">>),
                estree:literal(list_to_binary(Id)),
                true
            ),
            F
        ),
        parse_node(ReturnAtom, Params, Apply)
    ).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Parse a message receive block -------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
%% TODO: This is still a work in progress, it will not produce viable JS.
%% Code to be executed after the receive is passed in the parameters var.
parse_receive(ReturnAtom, Params, {c_receive,_,Clauses,Timeout,TimeoutConsequent})->
    case Params of
        {next,{A,B,C}} -> Next = 
            [estree:if_statement(
                estree:identifier(<<"__doNext">>),
                estree:call_expression(
                    estree:member_expression(
                        estree:identifier(<<"this">>),
                        estree:identifier(<<"setBehavior">>),
                        false
                    ),
                    [function_wrap([],parse_node(A,B,C))]
                ),null
            )],
            RealParams = B;
        _ -> Next = [],
            RealParams = Params
    end,
    [
        estree:variable_declaration([estree:variable_declarator(
            estree:identifier(<<"__mIndex">>),estree:literal(0))],
            <<"let">>
        ),
        estree:call_expression(
            estree:member_expression(
                estree:identifier(<<"this">>),
                estree:identifier(<<"setBehavior">>),
                false
            ),
            [
                function_wrap([],
                    [
                        estree:variable_declaration([
                            estree:variable_declarator(
                                estree:identifier(<<"__doNext">>),estree:literal(true)),
                            estree:variable_declarator(
                                estree:identifier(<<"__doLoop">>),estree:literal(false))],
                            <<"let">>
                        ),

                        estree:do_while_statement(
                            estree:identifier(<<"__doLoop">>),
                            estree:block_statement([
                                estree:expression_statement(
                                    estree:assignment_expression(
                                        <<"=">>,
                                        estree:identifier(<<"__doLoop">>),
                                        estree:literal(false)
                                    )
                                ),
                                estree:if_statement(
                                    estree:binary_expression(<<">=">>,
                                        estree:identifier(<<"__mIndex">>),
                                        estree:member_expression(
                                            estree:member_expression(
                                                estree:this_expression(),
                                                estree:identifier(<<"messages">>),
                                                false
                                            ),
                                            estree:identifier(<<"length">>),
                                            false
                                        )
                                    ),
                                    estree:block_statement([
                                        estree:expression_statement(
                                            estree:call_expression(
                                                estree:member_expression(
                                                    estree:this_expression(),
                                                    estree:identifier(<<"restartBehaviour">>),
                                                    false
                                                ),[]
                                            )
                                        ),
                                        estree:expression_statement(
                                            estree:assignment_expression(
                                                <<"=">>,
                                                estree:identifier(<<"__doNext">>),
                                                estree:literal(false)
                                            )
                                        )
                                    ]),
                                    estree:block_statement([
                                        estree:variable_declaration([
                                            estree:variable_declarator(
                                                estree:identifier(<<"__message">>),
                                                estree:member_expression(
                                                    estree:member_expression(
                                                        estree:this_expression(),
                                                        estree:identifier(<<"messages">>),
                                                        false
                                                    ),
                                                    estree:identifier(<<"__mIndex">>),
                                                    true
                                                )
                                            )],
                                            <<"let">>
                                        ),
                                        parse_receive_clauses(
                                            ReturnAtom,
                                            RealParams,
                                            [{c_var, a, '__message'}],
                                            Clauses)
                                    ])
                                )
                                
                            ])
                        )
                    |Next]
                )
            ]
        )
    ].

% estree:call_expression(
%     estree:member_expression(
%         estree:identifier(<<"TODO">>),
%         estree:identifier(<<"receive_placeholder">>),
%         false
%     ),[]
% )





%%% ---------------------------------------------------------------------------------------------%%%
%%% - Parse receive clauses ---------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
parse_receive_clauses(ReturnAtom, Params, Vars, []) ->
    [];
parse_receive_clauses(ReturnAtom, Params, Vars,
    [{c_clause, _, Match, Evaluate, Consequent}|Clauses]) ->
    % Parse the remaining clauses first.
    ElseClauses = parse_receive_clauses(ReturnAtom, Params, Vars, Clauses),
    % If there are no remaining clauses then use null
    case ElseClauses of
        [] -> ElseClausesActual = estree:block_statement([
                estree:expression_statement(
                    estree:update_expression(
                        <<"++">>,
                        estree:identifier(<<"__mIndex">>),
                        false
                    )
                ),
                estree:expression_statement(
                    estree:assignment_expression(
                        <<"=">>,
                        estree:identifier(<<"__doLoop">>),
                        estree:literal(true)
                    )
                )
            ]);
        _  -> ElseClausesActual = ElseClauses
    end,
    % Assemble the if statement
    estree:if_statement(
        % The function that serves as pattern patching and guards
        assemble_case_condition(Params, Vars, Match, Evaluate),
        estree:block_statement(
            assemble_sequence(
                estree:expression_statement(
                    estree:call_expression(
                        estree:member_expression(
                            estree:member_expression(
                                estree:this_expression(),
                                estree:identifier(<<"messages">>),
                                false
                            ),
                            estree:identifier(<<"splice">>),
                            false
                        ),
                        [estree:identifier(<<"__mIndex">>),
                        estree:literal(1)]
                    )
                ),
                assemble_sequence(
                    % Assign the variables that are used in the match
                    % At this point in development it should be un-nessisary to filter for un-parsed
                    % variables, but I'll leave it here until I next review the case clauses.
                    lists:filter(fun(Elem) ->
                            case Elem of
                                ok -> false;
                                _  -> true
                            end
                        end,
                        assign_matched_vars(Params, Vars, Match)
                    ),
                    encapsulate_expressions(
                        list_check(
                            parse_node(ReturnAtom, Params, Consequent)
                        )
                    )
                )
            )
        ),
        ElseClausesActual %alternate
    ).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Parse a case block's opening nodes --------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
%% For simplicity cases of a single var are converted to multi-var case format
parse_case(ReturnAtom, Params, {c_case, _, {c_var, _, Var}, Clauses}) ->
    parse_case(ReturnAtom, Params, {c_case, a, {c_values, a, [{c_var, a, Var}]}, Clauses});
parse_case(ReturnAtom, Params, {c_case, _, {c_values, _, Vars}, Clauses}) ->
    parse_case_clauses(ReturnAtom, Params, Vars, Clauses);

%% Cases that take a function call as an input are parsed here.
%% This one parses local functions.
parse_case(ReturnAtom, Params, {c_case, _, {c_apply, _, {c_var, _, Fun}, Args}, Clauses}) ->
    case Fun of
        {Name, _} -> Fun_Actual = Name;
        _ -> Fun_Actual = Fun
    end,
    parse_function_case(ReturnAtom, Params,
        estree:call_expression(
            estree:identifier(atom_to_binary(Fun_Actual, utf8)),
            lists:map(fun(T) -> parse_node(noreturn, Params, T) end, Args)
        ),
        Clauses
    );
%% And this one parses external functions.
parse_case(ReturnAtom, Params,
    {c_case, _, {c_call, _, {c_literal, _, Module}, {c_literal, _, FunctionName}, Args}, Clauses}
    ) ->
    parse_function_case(ReturnAtom, Params,
        estree:call_expression(
            estree:member_expression(
                estree:identifier(atom_to_binary(Module, utf8)),
                estree:identifier(atom_to_binary(FunctionName, utf8)),
                false
            ),
            lists:map(fun(T) -> parse_node(noreturn, Params, T) end, Args)
        ),
        Clauses
    ).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Parse a case block that takes a function --------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
%% In a break from the standard through the rest of the parsing functions, this function takes a
%% pre-parsed function call.
%% The result of the function is assigned to a temp variable ant that is used in a regular case
%% block.
parse_function_case(ReturnAtom, Params, FuncCall, Clauses) ->
    assemble_sequence(
        %Define temp variable & call function
        estree:variable_declaration([estree:variable_declarator(
            estree:identifier(atom_to_binary('_tempVar', utf8)),
            FuncCall
        )], <<"let">>),
        %Continue as normal, passing the temp variable
        parse_case(ReturnAtom, Params, {c_case, [], {c_var, [], '_tempVar'}, Clauses})
    ).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Parse case clauses ------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
parse_case_clauses(ReturnAtom, Params, Vars, []) ->
    [];
parse_case_clauses(ReturnAtom, Params, Vars,
    [{c_clause, _, Match, Evaluate, Consequent}|Clauses]) ->
    % Parse the remaining clauses first.
    ElseClauses = parse_case_clauses(ReturnAtom, Params, Vars, Clauses),
    % If there are no remaining clauses then use null
    case ElseClauses of
        [] -> ElseClausesActual = null;
        _  -> ElseClausesActual = ElseClauses
    end,
    % Assemble the if statement
     estree:if_statement(
        % The function that serves as pattern patching and guards
        assemble_case_condition(Params, Vars, Match, Evaluate),
        estree:block_statement(
            assemble_sequence(
                % Assign the variables that are used in the match
                % At this point in development it should be un-nessisary to filter for un-parsed
                % variables, but I'll leave it here until I next review the case clauses.
                lists:filter(fun(Elem) ->
                        case Elem of
                            ok -> false;
                            _  -> true
                        end
                    end,
                    assign_matched_vars(Params, Vars, Match)
                ),
                encapsulate_expressions(
                    list_check(
                        parse_node(ReturnAtom, Params, Consequent)
                    )
                )
            )
        ),
        ElseClausesActual %alternate
    ).
     % }.



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Make the clause condition self contained --------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
%% This allows the involved variables to be correctly scoped, and later defined.
%% This one shortcuts guard only matching
assemble_case_condition(Params, _, [], Evaluate) ->
    estree:call_expression(
        function_wrap(
            [],
            parse_node(return, Params, Evaluate)
        ),
    []);
assemble_case_condition(Params, Vars, Match, Evaluate) ->
    % This extracts the relevant variables for declaration, matching and assignment
    {DefL1, MatchL, DefL2} = lists:foldl(
        fun(Elem, {DefL1, MatchL, DefL2}) ->
            case Elem of
                {{c_var, _, ParamName}, {c_var, A, NewName}}->
                    {lists:append(DefL1, [
                        estree:variable_declarator(
                            estree:identifier(atom_to_binary(NewName, utf8)),
                            estree:identifier(atom_to_binary(ParamName, utf8))
                        )
                    ]), MatchL, DefL2};

                {V={c_var, _, ParamName}, M={c_literal, _, Literal}}->
                    {DefL1,
                    lists:append(MatchL,[
                        estree:call_expression(
                            estree:member_expression(
                                parse_node(noreturn,Params,V),
                                estree:identifier(<<"match">>),
                                false
                            ),
                            [parse_node(noreturn,Params,M)]
                        )
                    ]),DefL2};

                {V={c_var, _, ParamName}, M={c_cons, _, H, T}}->
                    {lists:append(DefL1,
                        recurse_var_declaration(M)
                    ),
                    lists:append(MatchL,[
                        estree:call_expression(
                            estree:member_expression(
                                parse_node(noreturn,Params,V),
                                estree:identifier(<<"match">>),
                                false
                            ),
                            [parse_node(noreturn,Params,M)]
                        )
                    ]),
                    lists:append(DefL2,recurse_var_assignments(
                            M,
                            estree:identifier(atom_to_binary(ParamName, utf8))
                    ))};

                {V={c_var, _, ParamName}, M={c_tuple, _, Content}}->
                    {lists:append(DefL1,
                        recurse_var_declaration(M)
                    ),
                    lists:append(MatchL,[
                        estree:call_expression(
                            estree:member_expression(
                                parse_node(noreturn,Params,V),
                                estree:identifier(<<"match">>),
                                false
                            ),
                            [parse_node(noreturn,Params,M)]
                        )
                    ]),
                    lists:append(DefL2, recurse_var_assignments(
                        M,
                        estree:identifier(atom_to_binary(ParamName, utf8))
                    ))};

                {V={c_var, _, ParamName}, {c_alias, _, {c_var, _, NewName}, M}}->
                    {lists:append(DefL1,
                        recurse_var_declaration(M)
                    ),
                    lists:append(MatchL,[
                        estree:call_expression(
                            estree:member_expression(
                                parse_node(noreturn,Params,V),
                                estree:identifier(<<"match">>),
                                false
                            ),
                            [parse_node(noreturn,Params,M)]
                        )
                    ]),
                    lists:append(DefL2, lists:append(
                        recurse_var_assignments(
                            M,
                            estree:identifier(atom_to_binary(ParamName, utf8))
                        ),
                        [
                            estree:assignment_expression(
                                <<"=">>,
                                estree:identifier(atom_to_binary(NewName, utf8)),
                                estree:identifier(atom_to_binary(ParamName, utf8))
                            )
                        ]
                    ))}
            end
        end,
        {[], [], []},
        lists:zip(Vars, Match)),
    % Check if any declarations are actually needed after matching
    case DefL2 of
        [] -> Declaration2Actual = [];
        _  -> Declaration2Actual = estree:variable_declaration(list_check(DefL2), <<"let">>)
    end,
    % Create the internal logic of the clause condition function. Including actual match calls and
    % the evaluation of the guard.
    Internal =  estree:if_statement(
                    assemble_match_calls(MatchL),
                    estree:block_statement(%consequent
                        encapsulate_expressions(
                            list_check(
                                assemble_sequence(
                                    Declaration2Actual,
                                    parse_node(return, Params, Evaluate)
                                )
                            )
                        )
                    ),
                    null
                ),
    % Check that variables actually need to be defined before matching is done
    case DefL1 of
        [] -> InternalActual = Internal;
        _  -> InternalActual = assemble_sequence(
                            estree:variable_declaration(list_check(DefL1), <<"let">>),
                            Internal
                        )
    end,
    % Finally return the call of the clause condition
    estree:call_expression(
        estree:call_expression(
            estree:member_expression(
                function_wrap(
                   [],
                   InternalActual
                ),
                estree:identifier(<<"bind">>),
                false),
            [estree:this_expression()]),
        []).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Recursively generate a list of variable declarations --------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
recurse_var_declaration({c_var, _, Name}) ->
    [estree:variable_declarator(
        estree:identifier(atom_to_binary(Name, utf8)),
        estree:identifier(<<"null">>)
    )];
recurse_var_declaration({c_literal, _, Name}) ->
    [];
recurse_var_declaration({c_tuple, _, Elements}) ->
    lists:foldl(fun(Elem, L) ->
            lists:append(L, recurse_var_declaration(Elem))
        end, [], Elements);
recurse_var_declaration({c_cons, _, A, B}) ->
    lists:append(recurse_var_declaration(A),
    recurse_var_declaration(B)).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Recursively generate a list of variable assignments ---------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
recurse_var_assignments(M, V) ->
    recurse_var_assignments(0, M, V, false).
recurse_var_assignments(_, {c_var, _, Name}, V, false) ->
    [
        estree:assignment_expression(
            <<"=">>,
            estree:identifier(atom_to_binary(Name, utf8)),
            V
        )
    ];
% Assign the remainder of a list
recurse_var_assignments(ConsCount, {c_var, _, Name}, V, isTail) ->
    [
        estree:assignment_expression(
            <<"=">>,
            estree:identifier(atom_to_binary(Name, utf8)),
            estree:call_expression(
                estree:member_expression(V, estree:identifier(<<"slice">>), false),
                [   estree:literal(ConsCount),
                    estree:call_expression(
                        estree:member_expression(V, estree:identifier(<<"size">>), false),
                        []
                    )
                ]
            )
        )
    ];
recurse_var_assignments(_, {c_tuple, _, Elements}, V, _) ->
    lists:foldl(
        fun(Elem, L) ->
            A = length(L),
            lists:append(
                L,
                recurse_var_assignments(0, Elem,
                    estree:member_expression(V, estree:literal(A), true),
                    false
                )
            )
        end,
        [], Elements);
% These two detect a list and begin counting the position in the list
% This one detects that a list constructor ends with a tail binding
recurse_var_assignments(ConsCount, {c_cons, _, A, B={c_var, _, Name}}, V, _) ->
    lists:append(
        recurse_var_assignments(0, A, estree:member_expression(V, estree:literal(ConsCount), true),
            false),
        recurse_var_assignments(ConsCount+1, B, V, isTail)
    );
recurse_var_assignments(ConsCount, {c_cons, _, A, B}, V, _) ->
    lists:append(
        recurse_var_assignments(0, A, estree:member_expression(V, estree:literal(ConsCount), true),
            false),
        recurse_var_assignments(ConsCount+1, B, V, false)
    );
recurse_var_assignments(_ConsCount, _, _V, _) ->
    [].



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Chain together all calls to match patterns ------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
assemble_match_calls(MatchL) ->
    case MatchL of
        []    -> estree:literal(true);
        [M]   -> M;
        [M|T] -> estree:logical_expression(<<"&&">>, M, assemble_match_calls(T))
    end.



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Assign variables after matching -----------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
% TODO: Review this when cases are next reviewed.
assign_matched_vars(Params, [V], [M]) ->
        assign_matched_vars(Params, V, M);
assign_matched_vars(Params, [V|Vars], [M|Match]) ->
    assemble_sequence(
        assign_matched_vars(Params, [V], [M]),
        assign_matched_vars(Params, Vars, Match)
    );
assign_matched_vars(Params, {c_var, _, Variable}, {c_var, _, Match}) ->
    [estree:expression_statement(
        estree:assignment_expression(
            <<"=">>,
            estree:identifier(atom_to_binary(Match, utf8)),
            estree:identifier(atom_to_binary(Variable, utf8))
        )
    )];
assign_matched_vars(Params, {c_var, _, Variable}, {c_alias, _, {c_var, [], Name}, _Value}) ->
    [estree:expression_statement(
        estree:assignment_expression(
            <<"=">>,
            estree:identifier(atom_to_binary(Name, utf8)),
            estree:identifier(atom_to_binary(Variable, utf8))
        )
    )];
%assign_matched_vars(Params, A, B) ->
assign_matched_vars(Params, _, _) ->
    [ok].



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Append two lists, even if they weren't lists to start with --------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
assemble_sequence(L, R) when is_list(L) and is_list(R) ->
    lists:append(L, R);
assemble_sequence(L, R) when is_list(R) ->
    [L|R];
assemble_sequence(L, R) when is_list(L) ->
    lists:append(L, [R]);
assemble_sequence(L, R) ->
    [L, R].



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Get the third value from each tuple for a list of tuples ----------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
tuple_list_get_vars_3([]) ->
    [];
tuple_list_get_vars_3([{_, _, Val} | Body]) ->
    [Val | tuple_list_get_vars_3(Body)];
tuple_list_get_vars_3([{_, _, Val, _} | Body]) ->
    [Val | tuple_list_get_vars_3(Body)].



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Get the second value from each tuple for a list of tuples ---------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
tuple_list_get_vars_2([]) ->
    [];
tuple_list_get_vars_2([{_, Val} | Body]) ->
    [Val | tuple_list_get_vars_2(Body)];
tuple_list_get_vars_2([{_, Val, _} | Body]) ->
    [Val | tuple_list_get_vars_2(Body)].



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Get the first value from each tuple for a list of tuples ----------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
tuple_getVar_1({V}) -> V;
tuple_getVar_1({V, _}) -> V;
tuple_getVar_1({V, _, _}) -> V;
tuple_getVar_1({V, _, _, _}) -> V;
tuple_getVar_1({V, _, _, _, _}) -> V;
tuple_getVar_1({V, _, _, _, _, _}) -> V;
tuple_getVar_1({V, _, _, _, _, _, _}) -> V.



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Create a list of variable declarators from a variable list --------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
declarators_from_list(List) ->
    lists:filtermap(fun(Elem) ->
        case Elem of
            {c_var, _, Name} -> { true, {Name,
                                estree:variable_declarator(
                                    estree:identifier(atom_to_binary(Name, utf8)),
                                    estree:identifier(<<"null">>))}};
            {c_alias, _, {c_var, _, Name}, Value} -> {true, {Name,
                                                 estree:variable_declarator(
                                                     estree:identifier(atom_to_binary(Name, utf8)),
                                                     parse_node(noreturn, [], Value))}};
            _              -> false
        end
    end, List).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Convert a tuple to a list -----------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
tup_to_list(Tuple) -> tup_to_list(Tuple, 1, tuple_size(Tuple)).

tup_to_list(Tuple, Pos, Size) when Pos =< Size ->
    [element(Pos, Tuple) | tup_to_list(Tuple, Pos+1, Size)];
tup_to_list(_Tuple, _Pos, _Size) -> [].



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Wrap one or more nodes in a function, with an optional list of parameters -----------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
function_wrap(Identifier_list,Body_nodes) ->
estree:function_expression(
        null,
        Identifier_list,
        estree:block_statement(
            encapsulate_expressions(
                list_check(
                    Body_nodes
                )
            )
        ),
        false
    ).


%%% ---------------------------------------------------------------------------------------------%%%
%%% - Check the parsed input is a Statement or Declaration --------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
%% Helper function to determine whether a given ESTree node is a statement
is_statement({_, Type, _}) ->
    case re:run(atom_to_list(Type), "Statement|Declaration") of
        {match, _} ->
            true;
        _ ->
            false
    end;
is_statement(_) ->
    false.



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Encapsulate any non-Expression in a list --------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
encapsulate_expressions(L) ->
    lists:map(
        fun(X) ->
            IsStmt = is_statement(X),
            if
                IsStmt->X;
                true->estree:expression_statement(X)
            end
        end,
        L
    ).



%%% ---------------------------------------------------------------------------------------------%%%
%%% - Check that the input is a list, but also not a list within a list -------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%
list_check([L]) when is_list(L) ->%un-nest lists
    list_check(L);
list_check(L) ->
    IsList = is_list(L),
    if
        IsList->L;
        true->[L]
    end.