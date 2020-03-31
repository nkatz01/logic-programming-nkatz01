tokenize(`1 * 3`, Tokens), untokenize(Tokens, Text), Tokens = [A,_,B,_,C|T].
tokenize(`1 * 3`, Tokens), untokenize(Tokens, Text), Tokens = [A,_,B,_,C],A = number(X), C = number(Z), B=punct((Y)),Ans is  X * Z.
 tokenize(`1 * 3`, Tokens), untokenize(Tokens, Text), Tokens = [A,_,B,_,C],A = number(X), C = number(Z), untokenize([B],Y),Y = [H], char_code(M,H), Ans is  X H Z.	
 
  tokenize(`2 * 3`, Tokens), Tokens = [A,_,B,_,C],A = number(X), C = number(Z), untokenize([B],[Y]),  M =..[Y,X,Z],  call(M).

%works
tokenize(`4 * 3`, Tokens , [cased(true), spaces(false)]),Tokens = [A,B,C], arg(1,A,X), B = punct(Y), arg(1,C,Z) , M =..[Y,X,Z], Ans is M.

%from file
tokenize_file('/mmExamples/example1.cmm', Tokens, [cased(true), spaces(false)]).

%also works
tokenize(`4 * 3 - 7 asdfdsf`, Tokens, [cased(true), spaces(false)]),Tokens = [A|T],functor(A,Name,NoOfParams),Name==number, arg(1,A,Int).

 node(program, nill, node(
							node(assign, node(number, nill, nill), node(100, nill, nill)), nill, [])).

statement(print(statements([W|Ww])))  --> [word(print)] -> [word(W)],  statement(Ww).						 	
statement([W|Ww]) --> [punct(,)] -> [word(W)], statement(Ww).
statement([W|Ww]) --> [punct(,)] -> [word(W)],rest_statements(Ww).

program(print(statements([['I', will|4]])), program(assign(id(number), number(100)), program(print(statements([[laugh, in|class]])), []))),

expr(((+), 3), expr(/, number(12), number(4)), number(6))) 

expression(expr(O2,expr(O, X, Z),Y)) --> pl_constant(X), arithmetic_op(Op),  {Op = (O,4)}, pl_constant(Z),  arithmetic_op(Op2) ,{Op2 = (O2,Whatever)} , expression(Y).
expression(expr(O, X, Y)) --> pl_constant(X), arithmetic_op(Op),  {Op = (O,3)}, expression(Y).
%expression((X,expr(O, Y))) -->  arithmetic_op(Op), {Op = (O,3)},expression(Y).
%expression((X,expr(O, Y))) --> pl_constant(X), arithmetic_op(Op), {Op = (O,4)},expression(Y).
expression(X)              --> pl_constant(X).

%if current op is high precedence and we receive from low precendence 
%rest_expression(expr(O, X, NewY)) --> expression(Y), Y = (X,NewY).
%if current op is low precedence and we receive from low precendence 
%rest_expression(expr(O, X, NewY)) --> expression(Y), Y = (X,NewY).

arithmetic_op((+,3))         --> [punct(+)].
arithmetic_op((-,3))         --> [punct(-)].
arithmetic_op((*,4))         --> [punct(*)].
arithmetic_op((/,4))         --> [punct(/)].
arithmetic_op((mod,4))         --> [punct('%')].


expre(Z) --> term((Op,X1,X2)), {X =..[Op, X1,X2]}, [+], expre(Y), {Z = X + Y}.%covers any of the below on the left - any of the below (eg (int +/-int)  * ((int +/-int) / (int +/-int)) -/+ (int +/-int)  * ((int +/-int) * (int +/-int)) 
expre(Z) --> term((Op,X1,X2)), {X =..[Op, X1,X2]}, [-], expre(Y), {Z = X - Y}. %or any of the below on the left with any of this own set on the right.
expre(X) --> term((Op,X1,X2)), {X =..[Op, X1,X2]}. %coevrs any of the below

term((Op,Z,Y2)) --> numb(X),  [*], term((Op,Y1,Y2)), {Z = X * Y1} .%covers  (int +/-int)  * ((int +/-int) / (int +/-int)) , 		(int +/-int)  * (int +/-int) * (int +/-int)	,		(int +/-int)  * (int +/-int)
term((Op,Z,Y2)) --> numb(X), [/], term((Op,Y1,Y2)), {Z = X / Y1}. %.covers  (int +/-int)  * ((int +/-int) * (int +/-int)) , 		(int +/-int)  / (int +/-int) / (int +/-int)	,		(int +/-int)  / (int +/-int)
term((-,Z,0)) --> numb(Z).

numb(C) --> [+], numb(C).%covers int + int, int - int, just int
numb(C) --> [-], numb(C).
numb(C) --> [C].



expre(Z) --> term(X), [+], expre(Y), {Z = X + Y}.%covers any of the below on the left - any of the below (eg (int +/-int)  * ((int +/-int) / (int +/-int)) -/+ (int +/-int)  * ((int +/-int) * (int +/-int)) 
expre(Z) --> term(X), [-], expre(Y), {Z = X - Y}. %or any of the below on the left with any of this own set on the right.
expre(X) --> term(X). %coevrs any of the below

term(Z) --> numb(X), [*], term(Y), {Z = X * Y}.%covers  (int +/-int)  * ((int +/-int) / (int +/-int)) , 		(int +/-int)  * (int +/-int) * (int +/-int)	,		(int +/-int)  * (int +/-int)
term(Z) --> numb(X), [/], term(Y), {Z = X / Y}.%covers  (int +/-int)  * ((int +/-int) * (int +/-int)) , 		(int +/-int)  / (int +/-int) / (int +/-int)	,		(int +/-int)  / (int +/-int)
term(Z) --> numb(Z).

numb(C) --> [+], numb(C).%covers int + int, int - int, just int
numb(C) --> [-], numb(C).
numb(C) --> [C].


%myeval(expr(Op,number(N1),number(N2)),Ans) :- Ans =.. [Op ,N1, N2].
%myeval(expr(Op,number(N),Expr),Ans) :- myeval(Expr,RestAns), Ans =..[Op,N, RestAns]  .

%myeval(expr(Op,number(N1),number(N2)),Ans) :- functor(Op,Name,_),  Ans =.. [Name, N1,N2].
%myeval(expr(Op,number(N),Expr),Ans) :- functor(Op,Name,_), Ans =..[Name,N, RestAns] , myeval(Expr,RestAns).