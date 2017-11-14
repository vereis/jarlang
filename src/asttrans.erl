% Author: Andrew Johnson
% Walks down a CoreErlang AST and calls EStree.

-module(asttrans).
-export([erast2esast/1]).

% Compiles a given Erlang source file to a EStree ast
erast2esast(AST) ->
    toksModule(AST).

%Read the module token (first token)
toksModule({c_module, _A, {_, _, ModuleName}, Exports, _Attributes, Functions})->
    io:format("module: ~s ~n", [ModuleName]),
    io:format("    exports: ~p ~n", [tupleList_getVars_3(Exports)]),
    FormattedFunctions = toksFunctions(Functions),
    FormattedExports = lists:map(fun({N,A})->{atom_to_list(N),A} end,tupleList_getVars_3(Exports)),
    %io:format("~p~n~n~n~n~n~n", [FormattedFunctions]),
    esast:c_module(atom_to_list(ModuleName),FormattedExports,FormattedFunctions);
    
    
toksModule(T)->
    io:format("Unrecognised Token in module section: ~p", [T]).



%Split functions apart
toksFunctions(Functions)->
    lists:map(fun(X)->toksFunc(X) end,Functions).

%Read a function
toksFunc({{_, _, {FunctionName, Arity}}, {_, [compiler_generated], _, _}})->
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),esast:functionExpression(null,[],esast:blockStatement([esast:emptyStatement()]),false)};
toksFunc({{_, _, {FunctionName, Arity}}, {_, _, ParamNames, Body}})->
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),esast:functionExpression(
        null,
        tupleListToIdentifierList(ParamNames),
        esast:blockStatement(
            encapsulateExpressions(
                listCheck(
                    toksFuncBody(return,Body)
                )
            )
        ),
        false
    )}.

encapsulateExpressions(L)->
    lists:map(
        fun(X)->
            IsStmt = esast:is_statement(X),
            if
                IsStmt->X;
                true->esast:expressionStatement(X)
            end
        end,
        L
    ).

listCheck([L]) when is_list(L) ->
    listCheck(L);
listCheck(L)->
    IsStmt = esast:is_statement(L),
    if
        IsStmt->[L];
        true->L
    end.


%Parse the function body

toksFuncBody(return,{c_call, A, {B, C, Module}, {D, E, FunctionName}, Params})->
    %io:format("        call function ~s:~s(", [Module, FunctionName]),
    %io:format("~p)~n", [tupleList_getVars_3(Params)]);
    esast:returnStatement(toksFuncBody(noreturn,{c_call, A, {B, C, Module}, {D, E, FunctionName}, Params}));



%Handle Operator calls



toksFuncBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'band'}, [L,R]})->
    esast:binaryExpression(atom_to_binary('&',utf8),toksFuncBody(noreturn,L),toksFuncBody(noreturn,R));
toksFuncBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'bor'}, [L,R]})->
    esast:binaryExpression(atom_to_binary('|',utf8),toksFuncBody(noreturn,L),toksFuncBody(noreturn,R));
toksFuncBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'bxor'}, [L,R]})->
    esast:binaryExpression(atom_to_binary('^',utf8),toksFuncBody(noreturn,L),toksFuncBody(noreturn,R));
toksFuncBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'bnot'}, [L]})->
    esast:unaryExpression(atom_to_binary('~',utf8),true,toksFuncBody(noreturn,L));

toksFuncBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'or'}, [L,R]})->
    esast:logicalExpression(atom_to_binary('||',utf8),toksFuncBody(noreturn,L),toksFuncBody(noreturn,R));
toksFuncBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'and'}, [L,R]})->
    esast:logicalExpression(atom_to_binary('&&',utf8),toksFuncBody(noreturn,L),toksFuncBody(noreturn,R));
toksFuncBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'not'}, [L]})->
    esast:unaryExpression(atom_to_binary('!',utf8),true,toksFuncBody(noreturn,L));

toksFuncBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'xor'}, [L,R]})->
    toksFuncBody(noreturn,{c_call, a, {a, a, erlang}, {a, a, 'or'}, [
            {c_call, a, {a, a, erlang}, {a, a, 'and'}, [L,{c_call, a, {a, a, erlang}, {a, a, 'not'}, [R]}]},
            {c_call, a, {a, a, erlang}, {a, a, 'and'}, [R,{c_call, a, {a, a, erlang}, {a, a, 'not'}, [L]}]}
        ]}
    );

toksFuncBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'div'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"Math">>),esast:identifier(<<"floor">>),false),
         [toksFuncBody(noreturn,{c_call, a, {b, c, erlang}, {d, e, '/'}, [L,R]})]
     );
toksFuncBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'rem'}, [L,R]})->
    esast:binaryExpression(atom_to_binary('%',utf8),toksFuncBody(noreturn,L),toksFuncBody(noreturn,R));
toksFuncBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '/='}, [L,R]})->
    esast:binaryExpression(atom_to_binary('!=',utf8),toksFuncBody(noreturn,L),toksFuncBody(noreturn,R));
toksFuncBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '=<'}, [L,R]})->
    esast:binaryExpression(atom_to_binary('<=',utf8),toksFuncBody(noreturn,L),toksFuncBody(noreturn,R));

toksFuncBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, FunctionName}, [L,R]})->
    esast:binaryExpression(atom_to_binary(FunctionName,utf8),toksFuncBody(noreturn,L),toksFuncBody(noreturn,R));

%Bodge job: external module calls
%TODO: proper external module calls
toksFuncBody(noreturn,{c_call, _, {_, _, io}, {_, _, format}, [T]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"console">>),esast:identifier(<<"log">>),false),
         [toksFuncBody(noreturn,T)]
     );


toksFuncBody(noreturn,{c_call, _, {_, _, Module}, {_, _, FunctionName}, Params})->
    io:format("",[]);


toksFuncBody(return,{c_values, _, Values})->
    %io:format("~p~n", [tupleList_getVars_3(Values)]);
    io:format("",[]);

toksFuncBody(return,{c_var, A, Var})->
    esast:returnStatement(toksFuncBody(noreturn,{c_var, A, Var}));
toksFuncBody(noreturn,{c_var, _, Var})->
    esast:identifier(atom_to_binary(Var,utf8));
    %io:format("",[]);


toksFuncBody(return,{c_seq, _, A, B})->
    assembleSequence(
        toksFuncBody(noreturn,A),
        toksFuncBody(return,B));
toksFuncBody(noreturn,{c_seq, _, A, B})->
    assembleSequence(
        toksFuncBody(noreturn,A),
        toksFuncBody(noreturn,B));



toksFuncBody(return,{c_let, _, [{_, _, Variable}], A, B})->
    %io:format("        Let statement: ~s~n", [Variable]),
    %toksFuncBody(A),
    %toksFuncBody(B);
    io:format("",[]);
    
% Is apply a local function call? Assignment from function? Assignment with pattern matching?
toksFuncBody(return,{c_apply, _, {_,_,{FName,_Arity}}, Params})->
    %io:format("Call local function ~s(",[FName]),
    %io:format("~p)~n", [tupleList_getVars_3(Params)]);
    io:format("",[]);

toksFuncBody(return,{c_apply, _, _A, _B})->
    %io:format("        Apply statement: ~n");
    io:format("",[]);
    
toksFuncBody(return,{c_literal,_,Value})->
    esast:returnStatement(esast:literal(Value));
toksFuncBody(_,{c_literal,_,Value})->
    esast:literal(Value);

toksFuncBody(return,{c_tuple,_,Values})->
    %io:format("        Tuple ~p~n", [tupleList_getVars_3(Values)]);
    io:format("",[]);
    
toksFuncBody(return,{c_primop,_,{_,_,Type},Details})->
    % io:format("        Error? ~p~n~p~n", [Type,Details]),
    esast:error(atom_to_list(Type),"TODO Errors dont parse nicely",esast:literal("Message"));
    % io:format("",[]);
    
toksFuncBody(return,{c_case, _, Condition, Clauses})->
    %io:format("        case statement:"),
    %toksFuncBody(noreturn,Condition),
    %lists:map(fun({c_clause,_,MatchVals,_true,Body})->toksFuncBody(return,Body) end,Clauses).
    io:format("",[]);
    
toksFuncBody(_,T)->
    io:format("Unrecognised Token in function body: ~p", [T]).

assembleSequence(L,R) when is_list(L) and is_list(R)->
    lists:append(L,R);
assembleSequence(L,R) when is_list(R)->
    [L|R];
assembleSequence(L,R) when is_list(L)->
    lists:append(L,[R]);
assembleSequence(L,R)->
    [L,R].


tupleListToIdentifierList(List)->
    lists:map(fun({c_var,[],A})->toksFuncBody(noreturn,{c_var,[],A}) end,List).



tupleList_getVars_3([])->
    [];
tupleList_getVars_3([{_,_, Val} | Body])->
    [Val | tupleList_getVars_3(Body)];
tupleList_getVars_3([{_, _, Val, _} | Body])->
    [Val | tupleList_getVars_3(Body)].

rAtomToList([A|Rest])->
    [rAtomToList(A)|rAtomToList(Rest)];
rAtomToList({A})->
    {rAtomToList(A)};
rAtomToList({A,B})->
    {rAtomToList(A),rAtomToList(B)};
rAtomToList({A,B,C})->
    {rAtomToList(A),rAtomToList(B),rAtomToList(C)};
rAtomToList({A,B,C,D})->
    {rAtomToList(A),rAtomToList(B),rAtomToList(C),rAtomToList(D)};
rAtomToList(A) when is_atom(A) ->
    atom_to_list(A);
rAtomToList(A) when not is_atom(A) ->
    A.









% Currently unused so commenting out for compilation
%tuple_to_string(T) ->
%   lists:flatten(io:format("~p", [T])).
