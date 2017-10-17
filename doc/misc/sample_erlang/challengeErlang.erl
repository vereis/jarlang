-module(challengeErlang).
-export([primeFactorsOf1806046/0,primeFactorsOf/1,
		emailAddressValidationTest/0,emailAddressValidation/1,
		multiThreadHelloWorld/0,r/0,r/1,
		tree/0,tree/1]).

multiThreadHelloWorld()->
	r().

%Print "Hello World" with each character printed from a separate thread.
%https://codegolf.stackexchange.com/a/5904
r()->r("Hello World\n").
r([])->'';r([H|T])->spawn(challengeErlang,r,[T]),io:format("~c",[H]).


tree(N)->
	t(N).
tree()->
	t(11).

%Print an ascii-art christmas tree.
%https://codegolf.stackexchange.com/a/4278
t(N)->[H|_]=T=t(N,1),io:format([T,H]).
t(0,_)->[];t(N,M)->[[d(N,32),d(M,42),10]|t(N-1,M+2)].
d(N,C)->lists:duplicate(N,C).


primeFactorsOf1806046()->
	primeFactorsOf(1806046).

primeFactorsOf(N)->
	string:join([integer_to_list(X) || X <- f(N)], "x").

%Prime Factors of a number
%https://stackoverflow.com/a/1310146
f(N) -> f(N,2,[]).
f(1,_,L) -> lists:reverse(L);
f(N,P,L) when N rem P == 0 -> f(N div P,P,[P|L]);
f(N,P,L) -> f(N,P+1,L).


emailAddressValidationTest()->
	true = 
	e("b@w.org") and
	e("r..t@x.tw") and
	e("j_r@x.mil") and
	not e("b@c@d.org") and
	not e("test@%.org") and
	not e("j_r@x.c.il") and
	not e("test@org") and
	not e("s%p@m.org") and
	not e("foo@a%.com").

emailAddressValidation(Email)->
	e(Email).

%Validate an Email Address without using regular expressions or similar.
%https://stackoverflow.com/a/1393945
-define(E,when X>=$a,X=<$z;X>=$A,X=<$Z).
-define(I(Y,Z),Y([X|L])?E->Z(L);Y(_)->false).
-define(L(Y,Z),Y([X|L])?E;X>=$0,X=<$9;X=:=$.;X=:=$_->Z(L);Y(_)->false).
?L(e,m).
m([$@|L])->a(L);?L(m,m).
?I(a,i).
i([$.|L])->l(L);?I(i,i).
?I(l,c).
?I(c,g).
g([])->true;?I(g,g).