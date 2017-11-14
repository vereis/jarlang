% Author: Andrew Johnson
% Walks down a CoreErlang AST and calls EStree.

-module(asttrans).
-export([erast2esast/1]).

% Compiles a given Erlang source file to a EStree ast
erast2esast(AST) ->
    parseModule(AST).

%Read the module token (first token)
parseModule({c_module, _A, {_, _, ModuleName}, Exports, _Attributes, Functions})->
    io:format("module: ~s ~n", [ModuleName]),
    io:format("    exports: ~p ~n", [tupleList_getVars_3(Exports)]),
    FormattedFunctions = parseFunctions(Functions),
    FormattedExports = lists:map(fun({N,A})->{atom_to_list(N),A} end,tupleList_getVars_3(Exports)),
    %io:format("~p~n~n~n~n~n~n", [FormattedFunctions]),
    esast:c_module(atom_to_list(ModuleName),FormattedExports,FormattedFunctions);
    
    
parseModule(T)->
    io:format("Unrecognised Token in module section: ~p", [T]).



%Split functions apart
parseFunctions(Functions)->
    lists:map(fun(X)->parseFunction(X) end,Functions).

%Read a function
parseFunction({{_, _, {FunctionName, Arity}}, {_, [compiler_generated], _, _}})->
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),esast:functionExpression(null,[],esast:blockStatement([esast:emptyStatement()]),false)};
parseFunction({{_, _, {FunctionName, Arity}}, {_, _, ParamNames, Body}})->
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),esast:functionExpression(
        null,
        tupleListToIdentifierList(ParamNames),
        esast:blockStatement(
            encapsulateExpressions(
                listCheck(
                    parseFunctionBody(return,Body)
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

parseFunctionBody(return,{c_call, A, {B, C, Module}, {D, E, FunctionName}, Params})->
    %io:format("        call function ~s:~s(", [Module, FunctionName]),
    %io:format("~p)~n", [tupleList_getVars_3(Params)]);
    esast:returnStatement(parseFunctionBody(noreturn,{c_call, A, {B, C, Module}, {D, E, FunctionName}, Params}));



%Handle Operator calls



parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'band'}, [L,R]})->
    esast:binaryExpression(atom_to_binary('&',utf8),parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R));
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'bor'}, [L,R]})->
    esast:binaryExpression(atom_to_binary('|',utf8),parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R));
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'bxor'}, [L,R]})->
    esast:binaryExpression(atom_to_binary('^',utf8),parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R));
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'bnot'}, [L]})->
    esast:unaryExpression(atom_to_binary('~',utf8),true,parseFunctionBody(noreturn,L));

parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'or'}, [L,R]})->
    esast:logicalExpression(atom_to_binary('||',utf8),parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R));
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'and'}, [L,R]})->
    esast:logicalExpression(atom_to_binary('&&',utf8),parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R));
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'not'}, [L]})->
    esast:unaryExpression(atom_to_binary('!',utf8),true,parseFunctionBody(noreturn,L));

parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'xor'}, [L,R]})->
    parseFunctionBody(noreturn,{c_call, a, {a, a, erlang}, {a, a, 'or'}, [
            {c_call, a, {a, a, erlang}, {a, a, 'and'}, [L,{c_call, a, {a, a, erlang}, {a, a, 'not'}, [R]}]},
            {c_call, a, {a, a, erlang}, {a, a, 'and'}, [R,{c_call, a, {a, a, erlang}, {a, a, 'not'}, [L]}]}
        ]}
    );

parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'div'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"Math">>),esast:identifier(<<"floor">>),false),
         [parseFunctionBody(noreturn,{c_call, a, {b, c, erlang}, {d, e, '/'}, [L,R]})]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'rem'}, [L,R]})->
    esast:binaryExpression(atom_to_binary('%',utf8),parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R));
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '/='}, [L,R]})->
    esast:binaryExpression(atom_to_binary('!=',utf8),parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R));
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '=<'}, [L,R]})->
    esast:binaryExpression(atom_to_binary('<=',utf8),parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R));

parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, FunctionName}, [L,R]})->
    esast:binaryExpression(atom_to_binary(FunctionName,utf8),parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R));

%Bodge job: external module calls
%TODO: proper external module calls
parseFunctionBody(noreturn,{c_call, _, {_, _, io}, {_, _, format}, [T]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"console">>),esast:identifier(<<"log">>),false),
         [parseFunctionBody(noreturn,T)]
     );


parseFunctionBody(noreturn,{c_call, _, {_, _, _Module}, {_, _, _FunctionName}, _Params})->
    io:format("",[]);


parseFunctionBody(return,{c_values, _, _Values})->
    %io:format("~p~n", [tupleList_getVars_3(Values)]);
    io:format("",[]);

parseFunctionBody(return,{c_var, A, Var})->
    esast:returnStatement(parseFunctionBody(noreturn,{c_var, A, Var}));
parseFunctionBody(noreturn,{c_var, _, Var})->
    esast:identifier(atom_to_binary(Var,utf8));
    %io:format("",[]);


parseFunctionBody(return,{c_seq, _, A, B})->
    assembleSequence(
        parseFunctionBody(noreturn,A),
        parseFunctionBody(return,B));
parseFunctionBody(noreturn,{c_seq, _, A, B})->
    assembleSequence(
        parseFunctionBody(noreturn,A),
        parseFunctionBody(noreturn,B));



parseFunctionBody(return,{c_let, _, [{_, _, _Variable}], _A, _B})->
    %io:format("        Let statement: ~s~n", [Variable]),
    %parseFunctionBody(A),
    %parseFunctionBody(B);
    io:format("",[]);
    
% Is apply a local function call? Assignment from function? Assignment with pattern matching?
parseFunctionBody(return,{c_apply, _, {_,_,{_FName,_Arity}}, _Params})->
    %io:format("Call local function ~s(",[FName]),
    %io:format("~p)~n", [tupleList_getVars_3(Params)]);
    io:format("",[]);

parseFunctionBody(return,{c_apply, _, _A, _B})->
    %io:format("        Apply statement: ~n");
    io:format("",[]);
    
parseFunctionBody(ReturnAtom,{c_literal,_,Value}) when is_atom(Value)->
    parseFunctionBody(ReturnAtom,{c_literal,a,atom_to_list(Value)});
parseFunctionBody(return,{c_literal,_,Value})->
    esast:returnStatement(esast:literal(Value));
parseFunctionBody(_,{c_literal,_,Value})->
    esast:literal(Value);

parseFunctionBody(return,{c_tuple,_,_Values})->
    %io:format("        Tuple ~p~n", [tupleList_getVars_3(Values)]);
    io:format("",[]);
    
parseFunctionBody(return,{c_primop,_,{_,_,Type},_Details})->
    % io:format("        Error? ~p~n~p~n", [Type,Details]),
    esast:error(atom_to_list(Type),"TODO Errors dont parse nicely",esast:literal("Message"));
    % io:format("",[]);


parseFunctionBody(ReturnAtom,{c_case, _, {c_var,_,Var}, Clauses})->
    %parseFunctionBody(noreturn,Condition),
    %lists:map(fun({c_clause,_,MatchVals,_true,Body})->parseFunctionBody(return,Body) end,Clauses).
    esast:switchStatement(
        parseFunctionBody(noreturn,{c_var,a,Var}),
        lists:map(
            fun({c_clause,_,[MatchVal],_trueLiteral,Body})->
                esast:switchCase(
                    parseFunctionBody(noreturn,MatchVal),
                    encapsulateExpressions(assembleSequence(
                        parseFunctionBody(ReturnAtom,Body),
                        esast:breakStatement(null)
                    ))
                )
            end,
            Clauses
        ),
        false
    );

parseFunctionBody(_,{c_case, _, _Condition, _Clauses})->
    io:format("Error: this type of case statement is not implemented"),
    io:format("",[]);


parseFunctionBody(_,T)->
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
    lists:map(fun({c_var,[],A})->parseFunctionBody(noreturn,{c_var,[],A}) end,List).



tupleList_getVars_3([])->
    [];
tupleList_getVars_3([{_,_, Val} | Body])->
    [Val | tupleList_getVars_3(Body)];
tupleList_getVars_3([{_, _, Val, _} | Body])->
    [Val | tupleList_getVars_3(Body)].

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
