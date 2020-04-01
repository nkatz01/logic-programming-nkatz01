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

%extractExp([[]],[]) :- 
%extractExp([Ast],Inputls) :- Ast =.. [Functor|InnerAst], (Functor == assign(ID,E), myeval(InnerAst,Inputls) , true); extractExp(InnerAst,Inputls).

runDef1(Tokens,Ast,Rest) :- my_tokenize('number = 4 + 6 / 2 * 12 - 5 print lola',Tokens) ,parse(Tokens, Ast,Rest).
%runPhar(Tokens,Ast,Rest) :- my_tokenize('(12+ 4)/6',Tokens),parse(Tokens, Ast,Rest) .

parse(Tokens, Ast,Rest) :-
  pl_program(Ast,Tokens,Rest).

%pl_program(['Program:'|S],Tokens) :- phrase(rest_statements(S),Tokens,Rest),!.%used together with the lists version
 

pl_program(S,Tokens,Rest)   :- phrase(rest_statements(S),Tokens,Rest),!.


statement(assign(id(X), E)) --> identifier(X) ->  [punct(=)] -> expression(E).

statement(print(statements([W])))  --> [word(print)] -> statement(W).						 	
statement([W|Ww]) --> [word(W)] ->  [punct(,)] , statement(Ww). 
statement([N|Ww]) --> [number(N)] -> [punct(,)]    , statement(Ww).
statement(W) --> [word(W)], statement. 
statement(N) --> [number(N)], statement. 
statement --> [].
%statement([W|Ww]),['}']  --> [punct(,)] -> [word(W)] -> [punct('}')],rest_statements(Ww).
 

%statement([W|Ww]),[X]  --> [punct(,)] ,rest_statements(Ww).%-> [word(X)]


rest_statements(program(S, Ss))    -->   statement(S), rest_statements(Ss).
rest_statements([])  --> [].


expression(expr(Op, X, Y)) --> pl_constant(X), arithmetic_op(Op), expression(Y).
expression(X)              --> pl_constant(X).


Ast =  (assign(id(number), expr(+, number(4), expr(/, number(6), expr(*, number(2), expr(-, number(12), number(5)))))), print(statements([lola])), [])



arithmetic_op(+)         --> [punct(+)].
arithmetic_op(-)         --> [punct(-)].
arithmetic_op(*)         --> [punct(*)].
arithmetic_op(/)         --> [punct(/)].
arithmetic_op(mod)         --> [punct('%')].

%phrase(expre(Z), [6,+,12,/,2]). not working
/* 
%driveEval(Ans) :- myeval(expr(+, number(4), expr(/, number(6), number(2))),Ans).
%expr(+, number(4), expr(/, number(6), expr(*, number(2), expr(-, number(12), number(5)))))

%expr(+, number(4), expr(/, number(6), number(2)))

 
out_order(X) --> [expr(Op,L,R)], Ans = L Op out_order(R).
out_order(nil) -->  .
        out_order(Right),
        [Name],
        out_order(Left).

%pl_program(['Program:'|S],Tokens) :- phrase(rest_statements(S),Tokens,Rest),!.%used together with the lists version



%statement([Ss])  -->   statement(Ss).%,  rest_statements(Ss).
%statement(['Assign:'|X]) --> [X] .
statement(['print:'|[W|Ww]])  --> [word(print)] -> [word(W)], statement(Ww).
statement([W|Ww]) --> [punct(,)] , [word(W)], statement(Ww).
statement([W|Ww]) -->  [punct(,)] , [word(W)],rest_statements(Ww).
statement([W|Ww]) -->  [punct(W)] ,rest_statements(Ww).
statement([W|Ww]) -->  [number(W)] ,rest_statements(Ww).
statement([W|Ww]) -->  [cntrl(W)] ,rest_statements(Ww).

rest_statements([])  --> [].
rest_statements([W|Ww])    -->   statement(W), rest_statements(Ww).
*/


%----------------
%rest_statements([])  --> []. %[NotaWord], { NotaWord}.
%statement([S|Ss])  --> ([word(S)]; [punct(S)]; [number(S)]) ,statement(Ss).%,  rest_statements(Ss). 








