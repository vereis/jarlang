%%% Module Description:
%%% Recursively descends into a core erlang AST and generates an ESTREE equivalent
-module(asttrans).
-author(["Andrew Johnson", "Chris Bailey"]).

-define(VERSION, "2.1.0").

-vsn(?VERSION).

%%% Export all functions if we compiled with erlc -dTEST or c(?MODULE, {d, 'TEST'}).
%%% This is so that we can run external eunit tests
-ifdef(TEST).
    -compile(export_all).
-else.
    -export([erast_to_esast/1]).
-endif.





%%% ---------------------------------------------------------------------------------------------%%%
%%% - MODULE ENTRYPOINTS ------------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Alternative entrypoint to parse_module/1
erast_to_esast(AST) ->
    parse_module(AST).

%% Begins parses ast expecting a c_module atom to indicate that the ast we're parsing
%% is indeed a core_erlang ast.
%% Processes information we need such as ModuleName and continues to parse functions and
%% module export information.
parse_module({c_module, _A, {_, _, ModuleName}, Exports, _Attributes, Functions})->
    FormattedFunctions = parse_functions(Functions),
    FormattedExports = lists:map(fun({N,A})->{atom_to_list(N),A} end,tuple_list_get_vars_3(Exports)),
    esast:c_module(atom_to_list(ModuleName),FormattedExports,FormattedFunctions);
parse_module(T)->
    io:format("Unrecognised Token in module section: ~p", [T]).





%%% ---------------------------------------------------------------------------------------------%%%
%%% - AST TO AST TRANSLATION --------------------------------------------------------------------%%%
%%% ---------------------------------------------------------------------------------------------%%%

%% Spawns new parse_function processes for each function in a given function list,
%% concurrently transpiling all functions given.
parse_functions(Functions)->
    Self = self(),
    Pids = lists:map(fun(X) ->
        spawn_link(fun() -> Self ! {self(), parse_function(X)} end)
    end, Functions),

    [
        receive
            {Pid, TranspiledFunction} ->
                TranspiledFunction
        end
        ||
        Pid <- Pids
    ].

%% Parse a function node, and recursively descend into the function to translate all
%% AST nodes into an JavaScript equivalent.
%% Compiler generated functions are currently generated as containing only an empty statement.
parse_function({{_, _, {FunctionName, Arity}}, {_c_fun, [compiler_generated], _, _}})->
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),estree:function_expression(null,[],estree:block_statement([estree:empty_statement()]),false)};
parse_function({{_, _, {FunctionName, Arity}}, {_c_fun, _, ParamNames, Body}})->
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),estree:function_expression(
        null,
        tuple_list_to_identifier_list(ParamNames,tuple_list_get_vars_3(ParamNames)),
        estree:block_statement(
            encapsulate_expressions(
                list_check(
                    parse_node(return,tuple_list_get_vars_3(ParamNames),Body)
                )
            )
        ),
        false
    )}.

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

%% Helper function to ensure that any given expression is a statement. If a statement
%% is given to this function, it is simply returned; otherwise we wrap it in an 
%% expression_statement.
encapsulate_expressions(L)->
    lists:map(
        fun(X)->
            IsStmt = is_statement(X),
            if
                IsStmt->X;
                true->estree:expression_statement(X)
            end
        end,
        L
    ).

%% I have no idea what this is meant to do
list_check([L]) when is_list(L) ->
    list_check(L);
list_check(L)->
    IsStmt = is_statement(L),
    if
        IsStmt->[L];
        true->L
    end.

%% Determines what type of node we're currently parsing and calls a function
%% to translate that specific node.
parse_node(ReturnAtom,Params,N)->
    case tuple_getVar_1(N) of
        c_call    -> parse_call(ReturnAtom,Params,N);
        c_values  -> parse_values(ReturnAtom,Params,N);
        c_var     -> parse_var(ReturnAtom,Params,N);
        c_seq     -> parse_seq(ReturnAtom,Params,N);
        c_let     -> parse_let(ReturnAtom,Params,N);
        c_apply   -> parse_apply(ReturnAtom,Params,N);
        c_literal -> parse_literal(ReturnAtom,Params,N);
        c_tuple   -> parse_tuple(ReturnAtom,Params,N);
        c_cons    -> parse_cons(ReturnAtom,Params,N);
        c_try     -> parse_try(ReturnAtom,Params,N);
        c_primop  -> parse_primop(ReturnAtom,Params,N);
        c_letrec  -> parse_letrec(ReturnAtom,Params,N);
        c_case    -> parse_case(ReturnAtom,Params,N)
    end.

%% Parses a c_call node.
parse_call(return,Params,{c_call, A, {B, C, Module}, {D, E, FunctionName}, Parameters})->
    estree:return_statement(parse_call(noreturn,Params,{c_call, A, {B, C, Module}, {D, E, FunctionName}, Parameters}));
parse_call(noreturn,Params,{c_call, _, {_, _, Module}, {_, _, FunctionName}, Parameters})->
    estree:call_expression(
         estree:member_expression(estree:identifier(atom_to_binary(Module,utf8)),estree:literal(atom_to_binary(FunctionName,utf8)),true),
         lists:map(fun(T)->parse_node(noreturn,Parameters,T) end,Parameters)
     ).

%% Parses a c_values node.
parse_values(return,Params,{c_values, _, _Values})->
    %io:format("~p~n", [tuple_list_get_vars_3(Values)]);
    io:format("",[]).

%% Parses a c_var node.
parse_var(return,Params,{c_var, A, Var})->
    estree:return_statement(parse_var(noreturn,Params,{c_var, A, Var}));
%added for support of functions by name
parse_var(noreturn,Params,{c_var, _, {Name,Arity}})->
    estree:identifier(atom_to_list(Name)++"/"++integer_to_list(Arity));
parse_var(noreturn,Params,{c_var, _, Var})->
    estree:identifier(atom_to_binary(Var,utf8)).
    %io:format("",[]);

%% Parses a c_seq node.
parse_seq(return,Params,{c_seq, _, A, B})->
    assemble_sequence(
        parse_node(noreturn,Params,A),
        parse_node(return,Params,B));
parse_seq(noreturn,Params,{c_seq, _, A, B})->
    assemble_sequence(
        parse_node(noreturn,Params,A),
        parse_node(noreturn,Params,B)).

%% Parses a c_let node.
%% A let statement is the core erlang representation of implicit variable declaration
%% Often the result of some function as an argument of another function.
parse_let(ReturnAtom,Params,{c_let, _, [{_, _, Variable}], Value, UsedBy})->
    assemble_sequence(
        estree:variable_declaration([estree:variable_declarator(estree:identifier(atom_to_binary(Variable,utf8)),parse_node(noreturn,Params,Value))],<<"let">>),
        parse_node(ReturnAtom,Params,UsedBy)).


%% Parses a c_apply node.
%% TODO: Is apply a local function call? 
%%       Assignment from function? 
%%       Assignment with pattern matching?
parse_apply(ReturnAtom,Params,{c_apply, _, {_,_,{FunctionName,Arity}}, Parameters})->
    % parse_node(ReturnAtom,Params,{c_call, [], {a, a, functions}, {a, a, FunctionName}, Parameters});
    parse_call(ReturnAtom,Params,{c_call, [], {a, a, functions}, {a, a, list_to_atom(atom_to_list(FunctionName)++"/"++integer_to_list(Arity))}, Parameters});
    % estree:call_expression(
    %      % estree:identifier(list_to_binary(atom_to_list(FunctionName)++"/"++integer_to_list(Arity))),
    %      estree:member_expression(
    %         estree:identifier(atom_to_binary(functions,utf8)),
    %         estree:literal(identify_normalise(atom_to_list(FunctionName)++"/"++integer_to_list(Arity))),
    %         true),
    %      lists:map(fun(T)->parse_node(noreturn,Parameters,T) end,Parameters)
    %  );
parse_apply(ReturnAtom,Params,{c_apply, _, {_, _, FunctionName}, Parameters})->
    parse_call(ReturnAtom,Params,{c_call, [], {a, a, functions}, {a, a, FunctionName}, Parameters}).

%% Parses a c_literal node.
parse_literal(return,Params,{c_literal,_,Value})->
    estree:return_statement(parse_literal(noreturn,Params,{c_literal,[],Value}));
parse_literal(noreturn,Params,{c_literal,_,Value}) when is_number(Value)->
    estree:new_expression(estree:identifier(<<"ErlNumber">>),[estree:literal(Value)]);
parse_literal(noreturn,Params,{c_literal,_,Value}) when is_atom(Value)->
    estree:new_expression(estree:identifier(<<"Atom">>),[estree:literal(atom_to_binary(Value, utf8))]);

parse_literal(noreturn,Params,{c_literal,_,Value}) when is_list(Value)->
    estree:new_expression(estree:identifier(<<"List">>),lists:map(fun(Elem)->
        parse_literal(noreturn,Params,{c_literal,[],Elem})
    end,Value));
parse_literal(noreturn,Params,{c_literal,_,Value}) when is_tuple(Value)->
    estree:new_expression(estree:identifier(<<"Tuple">>),
        case tup_to_list(Value) of
            [] -> [];
            Array -> [parse_literal(return,Params,{c_literal,[],A}) || A <- Array]
        end
    ).

%% Parses a c_tuple node.
parse_tuple(return,Params,A={c_tuple,_,Values})->
    estree:return_statement(parse_tuple(noreturn,Params,A));
parse_tuple(noreturn,Params,{c_tuple,_,Values})->
    estree:new_expression(estree:identifier(<<"Tuple">>),[parse_node(noreturn,Params,Value) || Value <- Values]).

%% Parses a c_cons node.
%% This is essentially a list constructor.
parse_cons(return,Params,{c_cons,_,A,B})->
    estree:return_statement(parse_cons(noreturn,Params,{c_cons,[],A,B}));
parse_cons(noreturn,Params,{c_cons,_,A,B={c_cons,_,C,D}})->
    estree:new_expression(estree:identifier(<<"List">>),
        parse_cons_chain(noreturn,Params,{c_cons,[],A,B})
   );
parse_cons(noreturn,Params,{c_cons,_,A,B})->
    estree:new_expression(estree:identifier(<<"List">>),[parse_node(noreturn,Params,A),parse_node(noreturn,Params,B)]).

%% Parses a c_try node.
%% For lack of a more apparent reason for the c_try token I'm treating it a superfluous encapsulation
parse_try(return,Params,{c_try,_,Elem,_,_,_,_})->
    estree:return_statement(parse_try(noreturn,Params,{c_try,[],Elem,a,a,a,a}));
parse_try(noreturn,Params,{c_try,_,Elem,_,_,_,_})->
    estree:call_expression(
        estree:function_expression(
            null,
            [],
            estree:block_statement(
                encapsulate_expressions(
                    list_check(
                        parse_node(return,Params,Elem)
                    )
                )
            ),
        false),
    []).
    % parse_node(ReturnAtom,Params,Elem);

%% Parses a c_primop node.
parse_primop(return,Params,{c_primop,_,{_,_,Type},_Details})->
    % io:format("        Error? ~p~n~p~n", [Type,Details]),
    estree:error(atom_to_list(Type),"TODO Errors dont parse nicely\\n",estree:literal(<<"Message">>)).
    % io:format("",[]);

%% Parses a c_letrec node.
%% Note: c_letrec appears to represent list comprehension.
parse_letrec(ReturnAtom,Params,{c_letrec,_,[Func],Apply})->
% parse_node(ReturnAtom,Params,{c_letrec,_,[{{_, _, {FunctionName, Arity}}, {_c_fun, _, ParamNames, Body}}],Apply})->
%     {Id,F} = parse_function({{a, [], {'listComp', Arity}}, {c_fun, [], ParamNames, Body}}),
    {Id,F} = parse_function(Func),
    assemble_sequence(
        %estree:variable_declaration(estree:variable_declarator(estree:identifier(list_to_binary(Id)),F),<<"let">>),
        % estree:let_declaration(list_to_binary(Id),F),
        estree:assignment_expression(
            <<"=">>,
            estree:member_expression(
                estree:identifier(<<"functions">>),
                estree:literal(identify_normalise(Id)),
                % estree:identifier(<<"listComp">>),
                true
            ),
            F
        ),
        % estree:function_expression(
        %     estree:identifier(identify_normalise(atom_to_list(FunctionName)++"/"++integer_to_list(Arity))),
        %     tuple_list_to_identifier_list(ParamNames,tuple_list_get_vars_3(ParamNames)),
        %     estree:block_statement(
        %         encapsulate_expressions(
        %             list_check(
        %                 parse_node(return,tuple_list_get_vars_3(ParamNames),Body)
        %             )
        %         )
        %     ),
        %     true
        % ),
        parse_node(ReturnAtom,Params,Apply)
    ).

%% Parses a c_case node.
%% converts single var case to multi-var case format
parse_case(ReturnAtom,Params,{c_case, _, {c_var,_,Var}, Clauses})->
    parse_case(ReturnAtom,Params,{c_case, a, {c_values,a,[{c_var,a,Var}]}, Clauses});
parse_case(ReturnAtom,Params,{c_case, _, {c_values,_,Vars}, Clauses})->
    parse_case_clauses(ReturnAtom,Params, Vars, Clauses);
    % {UnboundVars,CaseClauses} = parse_case_clauses(ReturnAtom,Params, Vars, Clauses),
    % case UnboundVars of
    %     [] -> CaseClauses;
    %     _  -> assemble_sequence(
    %             estree:variable_declaration(
    %                 tuple_list_get_vars_2(maps:to_list(maps:from_list(UnboundVars))),
    %                 <<"let">>),
    %             CaseClauses)
    % end;
parse_case(ReturnAtom,Params,{c_case, _, {c_apply,_,{c_var,_,Fun},Args}, Clauses})->
    case Fun of
        {Name,_} -> Fun_Actual = Name;
        _ -> Fun_Actual = Fun
    end,
    parse_function_case(ReturnAtom,Params,
        estree:call_expression(
            estree:identifier(atom_to_binary(Fun_Actual,utf8)),
            lists:map(fun(T)->parse_node(noreturn,Params,T) end,Args)
        ),
        Clauses
    );
parse_case(ReturnAtom,Params,{c_case, _, {c_call,_,{c_literal,_,Module},{c_literal,_,FunctionName},Args}, Clauses})->
    parse_function_case(ReturnAtom,Params,
        estree:call_expression(
            estree:member_expression(estree:identifier(atom_to_binary(Module,utf8)),estree:identifier(atom_to_binary(FunctionName,utf8)),false),
            lists:map(fun(T)->parse_node(noreturn,Params,T) end,Args)
        ),
        Clauses
    ).
parse_function_case(ReturnAtom,Params,FuncCall, Clauses)->
    assemble_sequence(
        %Define temp variable & call function
        estree:variable_declaration([estree:variable_declarator(
            estree:identifier(atom_to_binary('_tempVar',utf8)),
            FuncCall
        )],<<"let">>),
        %Continue as normal, passing the temp variable
        parse_case(ReturnAtom,Params,{c_case, [], {c_var,[],'_tempVar'}, Clauses})
    ).

%% Parses a c_cons chain.
parse_cons_chain(noreturn,Params,{c_cons,[],A,{c_cons,_,C,D}})->
    [parse_node(noreturn,Params,A)|parse_cons_chain(noreturn,Params,{c_cons,[],C,D})];
parse_cons_chain(noreturn,Params,{c_cons,[],A,B})->
    [parse_node(noreturn,Params,A),parse_node(noreturn,Params,B)].

%% Parses c_clause nodes.
parse_case_clauses(ReturnAtom,Params, Vars, [])->
    [];
parse_case_clauses(ReturnAtom,Params, Vars, [{c_clause,_,Match,Evaluate,Consequent}|Clauses])->
    % {UnboundVars,ElseClauses} = parse_case_clauses(ReturnAtom,Params, Vars, Clauses),%alternate
    ElseClauses = parse_case_clauses(ReturnAtom,Params, Vars, Clauses),%alternate
    case ElseClauses of
        [] -> ElseClausesActual = null;
        _  -> ElseClausesActual = ElseClauses
    end,
    % {lists:append(declarators_from_list(Match),UnboundVars),
     estree:if_statement(
        assemble_case_condition(Params,Vars,Match,Evaluate),%test
        estree:block_statement(%consequent
            assemble_sequence(
                lists:filter(fun(Elem)->
                        case Elem of
                            ok -> false;
                            _  -> true
                        end
                    end,
                    assign_matched_vars(Params,Vars,Match)
                ),
                encapsulate_expressions(
                    list_check(
                        parse_node(ReturnAtom,Params,Consequent)
                    )
                )
            )
        ),
        ElseClausesActual %alternate
    ).
     % }.

%% Puts together a case condition
assemble_case_condition(Params,_,[],Evaluate)->
    estree:call_expression(
         estree:function_expression(
              null,
              [],
              estree:block_statement(
                encapsulate_expressions(
                    list_check(
                        parse_node(return,Params,Evaluate)
                    )
                )
              ),
              false),
         []);
assemble_case_condition(Params,Vars,Match,Evaluate)->
    {DefL1,MatchL,DefL2} = lists:foldl(
        fun(Elem,{DefL1,MatchL,DefL2})->
            case Elem of
                {{c_var,_,ParamName},{c_var,A,NewName}}->
                    {lists:append(DefL1,[
                        estree:variable_declarator(estree:identifier(atom_to_binary(NewName,utf8)),estree:identifier(atom_to_binary(ParamName,utf8)))
                    ]),MatchL,DefL2};

                {V={c_var,_,ParamName},M={c_literal,_,Literal}}->
                    {DefL1,
                    lists:append(MatchL,[
                        parse_call(noreturn,Params,{c_call, a, {a, a, erlang}, {a, a, 'match'}, [M,V]})
                    ]),DefL2};

                {V={c_var,_,ParamName},M={c_cons,_,H,T}}->
                    {lists:append(DefL1,
                        recurse_var_declaration(M)
                    ),
                    lists:append(MatchL,[
                        parse_call(noreturn,Params,{c_call, a, {a, a, erlang}, {a, a, 'match'}, [M,V]})
                    ]),
                    lists:append(DefL2,recurse_var_assignments(M,estree:identifier(atom_to_binary(ParamName,utf8))))};

                {V={c_var,_,ParamName},M={c_tuple,_,Content}}->
                    {lists:append(DefL1,
                        recurse_var_declaration(M)
                    ),
                    lists:append(MatchL,[
                        parse_call(noreturn,Params,{c_call, a, {a, a, erlang}, {a, a, 'match'}, [M,V]})
                    ]),
                    lists:append(DefL2,recurse_var_assignments(M,estree:identifier(atom_to_binary(ParamName,utf8))))};

                {V={c_var,_,ParamName},{c_alias,_,{c_var,_,NewName},M}}->
                    {lists:append(DefL1,
                        recurse_var_declaration(M)
                    ),
                    lists:append(MatchL,[
                        parse_call(noreturn,Params,{c_call, a, {a, a, erlang}, {a, a, 'match'}, [M,V]})
                    ]),
                    lists:append(DefL2,lists:append(
                        recurse_var_assignments(M,estree:identifier(atom_to_binary(ParamName,utf8))),
                        [
                            estree:assignment_expression(
                                <<"=">>,
                                estree:identifier(atom_to_binary(NewName,utf8)),
                                estree:identifier(atom_to_binary(ParamName,utf8))
                            )
                        ]
                    ))}
            end
        end,
        {[],[],[]},
        lists:zip(Vars,Match)),
    Internal =  estree:if_statement(
                    assemble_match_calls(MatchL),
                    estree:block_statement(%consequent
                        encapsulate_expressions(
                            list_check(
                                assemble_sequence(
                                    estree:sequence_expression(DefL2),
                                    parse_node(return,Params,Evaluate)
                                )
                            )
                        )
                    ),
                    null
                ),
    case DefL1 of
        [] -> InternalActual = Internal;
        _  -> InternalActual = assemble_sequence(
                            estree:variable_declaration(list_check(DefL1),<<"let">>),
                            Internal
                        )
    end,
    estree:call_expression(
        estree:function_expression(
            null,
            [],
            estree:block_statement(
                encapsulate_expressions(
                    list_check(
                        InternalActual
                    )
                )
            ),
            false
        ),
    []).

recurse_var_declaration({c_var,_,Name})->
    [estree:variable_declarator(estree:identifier(atom_to_binary(Name,utf8)),estree:identifier(<<"undefined">>))];
recurse_var_declaration({c_literal,_,Name})->
    [];
recurse_var_declaration({c_tuple,_,Elements})->
    lists:foldl(fun(Elem,L)->
            lists:append(L,recurse_var_declaration(Elem))
        end,[],Elements);
recurse_var_declaration({c_cons,_,A,B})->
    lists:append(recurse_var_declaration(A),
    recurse_var_declaration(B)).

recurse_var_assignments(M,V)->
    recurse_var_assignments(0,M,V,false).
recurse_var_assignments(_,{c_var,_,Name},V,false)->
    [
        estree:assignment_expression(
            <<"=">>,
            estree:identifier(atom_to_binary(Name,utf8)),
            V
        )
    ];
recurse_var_assignments(ConsCount,{c_var,_,Name},V,isTail)->
    [
        estree:assignment_expression(
            <<"=">>,
            estree:identifier(atom_to_binary(Name,utf8)),
            estree:call_expression(
                estree:member_expression(V,estree:identifier(<<"slice">>),false),
                [   estree:literal(ConsCount),
                    estree:call_expression(estree:member_expression(V,estree:identifier(<<"size">>),false),[])
                ]
            )
        )
    ];
recurse_var_assignments(_,{c_tuple,_,Elements},V,_)->
    lists:foldl(
        fun(Elem,L)->
            A = length(L),
            lists:append(
                L,
                recurse_var_assignments(0,Elem,
                    estree:member_expression(V,estree:literal(A),true),
                    false
                )
            )
        end,
        [],Elements);
recurse_var_assignments(ConsCount,{c_cons,_,A,B={c_var,_,Name}},V,_)->
    lists:append(
        recurse_var_assignments(0,A,estree:member_expression(V,estree:literal(ConsCount),true),false),
        recurse_var_assignments(ConsCount+1,B,V,isTail)
    );
recurse_var_assignments(ConsCount,{c_cons,_,A,B},V,_)->
    lists:append(
        recurse_var_assignments(0,A,estree:member_expression(V,estree:literal(ConsCount),true),false),
        recurse_var_assignments(ConsCount+1,B,V,false)
    );
recurse_var_assignments(_ConsCount,_,_V,_)->
    [].

assemble_match_calls(MatchL)->
    % io:format("~p~n",[MatchL]),
    case MatchL of
        []    -> estree:literal(true);
        [M]   -> M;
        [M|T] -> estree:logical_expression(<<"&&">>,M,assemble_match_calls(T))
        % [M|T] -> estree:expression_statement(estree:logical_expression(<<"&&">>,M,assemble_match_calls(T)))
    end.

assign_matched_vars(Params,[V],[M])->
        assign_matched_vars(Params,V,M);
assign_matched_vars(Params,[V|Vars],[M|Match])->
    assemble_sequence(
        assign_matched_vars(Params,[V],[M]),
        assign_matched_vars(Params,Vars,Match)
    );
assign_matched_vars(Params,{c_var,_,Variable},{c_var,_,Match})->
    [estree:expression_statement(
        estree:assignment_expression(
            <<"=">>,
            estree:identifier(atom_to_binary(Match,utf8)),
            estree:identifier(atom_to_binary(Variable,utf8))
        )
    )];
assign_matched_vars(Params,{c_var,_,Variable},{c_alias,_,{c_var,[],Name},_Value})->
    [estree:expression_statement(
        estree:assignment_expression(
            <<"=">>,
            estree:identifier(atom_to_binary(Name,utf8)),
            estree:identifier(atom_to_binary(Variable,utf8))
        )
    )];
%assign_matched_vars(Params,A,B)->
%    erlang:error(io_lib:format("assignMatchedVars error:~p~n~p",[A,B])),
assign_matched_vars(Params,_,_)->
    [ok].

%###########################
assemble_sequence(L,R) when is_list(L) and is_list(R)->
    lists:append(L,R);
assemble_sequence(L,R) when is_list(R)->
    [L|R];
assemble_sequence(L,R) when is_list(L)->
    lists:append(L,[R]);
assemble_sequence(L,R)->
    [L,R].

tuple_list_to_identifier_list(List,Params)->
    lists:map(fun({c_var,[],A})->parse_var(noreturn,Params,{c_var,[],A}) end,List).

tuple_list_get_vars_3([])->
    [];
tuple_list_get_vars_3([{_,_, Val} | Body])->
    [Val | tuple_list_get_vars_3(Body)];
tuple_list_get_vars_3([{_, _, Val, _} | Body])->
    [Val | tuple_list_get_vars_3(Body)].

tuple_list_get_vars_2([])->
    [];
tuple_list_get_vars_2([{_, Val} | Body])->
    [Val | tuple_list_get_vars_2(Body)];
tuple_list_get_vars_2([{_, Val, _} | Body])->
    [Val | tuple_list_get_vars_2(Body)].

tuple_getVar_1({V})->V;
tuple_getVar_1({V,_})->V;
tuple_getVar_1({V,_,_})->V;
tuple_getVar_1({V,_,_,_})->V;
tuple_getVar_1({V,_,_,_,_})->V;
tuple_getVar_1({V,_,_,_,_,_})->V;
tuple_getVar_1({V,_,_,_,_,_,_})->V.

declarators_from_list(List)->
    lists:filtermap(fun(Elem)->
        case Elem of
            {c_var,_,Name} -> { true,{Name,
                                estree:variable_declarator(
                                    estree:identifier(atom_to_binary(Name,utf8)),
                                    estree:identifier(<<"undefined">>))}};
            {c_alias,_,{c_var,_,Name},Value} -> {true,{Name,
                                                 estree:variable_declarator(
                                                     estree:identifier(atom_to_binary(Name,utf8)),
                                                     parse_node(noreturn,[],Value))}};
            _              -> false
        end
    end,List).

identify_normalise(N)->
    % {ok, Regex}=re:compile("[^A-Za-z0-9_$]"),
    % iolist_to_binary(re:replace(N,Regex,"_$_",[global])).
    list_to_binary(N).

tup_to_list(Tuple) -> tup_to_list(Tuple, 1, tuple_size(Tuple)).

tup_to_list(Tuple, Pos, Size) when Pos =< Size ->  
    [element(Pos,Tuple) | tup_to_list(Tuple, Pos+1, Size)];
tup_to_list(_Tuple,_Pos,_Size) -> [].

%rAtomToList([A|Rest])->
%    [rAtomToList(A)|rAtomToList(Rest)];
%rAtomToList({A})->
%    {rAtomToList(A)};
%rAtomToList({A,B})->
%    {rAtomToList(A),rAtomToList(B)};
%rAtomToList({A,B,C})->
%    {rAtomToList(A),rAtomToList(B),rAtomToList(C)};
%rAtomToList({A,B,C,D})->
%    {rAtomToList(A),rAtomToList(B),rAtomToList(C),rAtomToList(D)};
%rAtomToList(A) when is_atom(A) ->
%    atom_to_list(A);
%rAtomToList(A) when not is_atom(A) ->
%    A.









% Currently unused so commenting out for compilation
%tuple_to_string(T) ->
%   lists:flatten(io:format("~p", [T])).
