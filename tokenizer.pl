% {write('im called')} ,
% {format('X=~w~n', [X])},
%format('~s~n', [Rest])
print_term(List,[]),nl.
tokenize(`1 * 3`, Tokens), untokenize(Tokens, Text), Tokens = [A,_,B,_,C|T].
tokenize(`1 * 3`, Tokens), untokenize(Tokens, Text), Tokens = [A,_,B,_,C],A = number(X), C = number(Z), B=punct((Y)),Ans is  X * Z.
 tokenize(`1 * 3`, Tokens), untokenize(Tokens, Text), Tokens = [A,_,B,_,C],A = number(X), C = number(Z), untokenize([B],Y),Y = [H], char_code(M,H), Ans is  X H Z.	
 
  tokenize(`2 * 3`, Tokens), Tokens = [A,_,B,_,C],A = number(X), C = number(Z), untokenize([B],[Y]),  M =..[Y,X,Z],  call(M).

%works
tokenize(`4 * 3`, Tokens , [cased(true), spaces(false)]),Tokens = [A,B,C], arg(1,A,X), B = punct(Y), arg(1,C,Z) , M =..[Y,X,Z], Ans is M.

%from file
tokenize_file(/cmmExamples/example1.cmm', Tokens, [cased(true), spaces(false)]).

%also works
tokenize(`4 * 3 - 7 asdfdsf`, Tokens, [cased(true), spaces(false)]),Tokens = [A|T],functor(A,Name,NoOfParams),Name==number, arg(1,A,Int).
tokenize(`4 * 3 - 7 asdfdsf`, Tokens, [cased(true), spaces(false)]),Tokens = [A|T],functor(BoundedVar,NameOfFunctor,NoOfParams),Name==number, arg(NthArgtoget,BoundedVar,BindsToTheArg).


/*filter(_,[],[]).
filter(P, A0-As0, As) :-
    (
        call(P, A0) -> As = A0-As1
    ;
        As = As1
    )
    , filter(P, As0, As1).

*/


are_identical(X, Y) :- %https://stackoverflow.com/questions/297996/prolog-filtering-a-list
    X == Y.

filterList(A, In, Out) :-
    exclude(are_identical(A), In, Out).

/*	
is_space_or_cntrl(X) :-  
    X == space(' '); X == cntrl(_).
	
filterList(In, Out) :-
    exclude(is_space_or_cntrl(X), In, Out).
*/

%filterCtrlsAndDblSpaces(In, Out2) :- filterList(space(' '),In,Out1), filterList(cntrl('\r'),Out1,Out2), filterList(cntrl('\n'),Out1,Out2),  filterList(cntrl('\t'),Out1,Out2).



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

 (assign(id(number),  (-, (+, num(4), mul, (div, num(6), num(2)), num(12)), num(5))), []),

val = 1

while (val < 100) {
    print val
    val = 2 * val 
}

 ((+), num(4), (*), num(1), num(12)))
 ((+), ((/), num(6), num(4)), (*), num(1), num(12)))
((-), ((+), ((+), num(4), num(6)), (\/), num(2), num(12)), num(5))
'number = 4 + 6 / 2 * 12 - 5 '
((*), ((\/), num(6), num(2)), num(12))

(assign(id(number),  ((+), num(4), num(6)))

(assign(id(number), num(4)), assign(id(eggs), num(6)), assign(id(total),  ((+), id(eggs), num(6)))
 T = (*,2,3), T =(Op,X,Y), Mul =.. [Op,X,Y], Ans is Mul.



(E = num(X); E = id(X), call(id(X,Y))) -> Res is Y;true




(E = id(V), call(id(V,D)) -> Res is D; true);
	(E = (Op,id(V),num(Y)),call(id(V,X)); 
	E = (Op, num(X), num(Y))-> Operate =.. [Op,X,Y] , Res is Operate; true),



extractExp([]). 
extractExp((LeftNode,RightTree)) :- 
	write(LeftNode),nl, write(RightTree),nl,
	(LeftNode = assign(_,E) -> 
	( E = num(D) -> Res is D );
	(E = id(V), call(id(V,D)) -> Res is D),
	
	write(Res),nl
	%evaluteExp(E,Results),
	%write(Results), nl
	; true  )
	, extractExp(RightTree),!.

evaluteExp(Tree,Res) :- 
	%Tree = (O,LL,RR),write(LL),nl, write(RR),nl;
	Tree = num(Res); (Tree = id(V), call(id(V,num(Res))));
(
	(Tree = (Op, num(X), num(Y))) ; 
	(Tree = (Op, id(V), num(Y)),  call(id(V,num(X)))) ;
	(Tree = (Op, num(X), id(V)), call(id(V,num(Y)))); 
	(Tree = (Op, id(V1), id(V2)), call(id(V1,num(X))),call(id(V2,num(Y)))))	, Operate =.. [Op,X,Y] , Res is Operate
	
.


replace_existing_fact(OldVar, NewVar) :-
    (   call(OldVar) 
	-> retractall(OldVar),
   assertz(NewVar);
      assertz(NewVar)
    ).%https://stackoverflow.com/questions/37871775/prolog-replace-fact-using-fact
 

replace_existing_fact(OldVar, NewVar) :-
       call(OldVar) ,
	retract(OldVar),
   assert(NewVar)
   
     .%https://stackoverflow.com/questions/37871775/prolog-replace-fact-using-fact
 
 
extractExp([]). 
extractExp((LeftNode,RightTree)) :-  									%( E = num(D) ;	(E = id(V), call(id(V,num(D))))), Res = D,	write(Res),nl)
	(LeftNode = assign(id(Id),E) ->
	evaluteExp(E,Results),	
	write(Results), nl,
	%write(Id), 
	replace_each_existing_fact(id(Id,_),id(Id,num(Results)))		%replace_existing_fact(id(Id,_),id(X,num(Results)))
	; true  )
	, extractExp(RightTree),!.




Sym = =<, Op =..[Sym, 4,5], call(Op). except for AND and OR



V is /\(1,1)



X=1,Y=1, V = /\(X,Y), V = (Op,X,Y).

evaluteExp(Tree,Res) :- 
	%Tree = (O,LL,RR),write(LL),nl, write(RR),nl;
	(Tree = num(Res); (Tree = id(V), traceID(V,Res)));
(
	((Tree = (Op, num(X), num(Y))) ; 
	(Tree = (Op, id(V), num(Y)), traceID(V,X)) ;
	(Tree = (Op, num(X), id(V)), traceID(V,Y)); 
	(Tree = (Op, id(V1), id(V2)),  traceID(V1,X),traceID(V2,Y))	),
	
	(is_logical_op(Op),is_one_or_zero(X,Y)) -> 
								(Op == /\  -> 
									Res is /\(X,Y)
									;
									Res is \/(X,Y)
								)
	;
	Operate =.. [Op,X,Y] ,
	(is_equality_op(Op); is_relat_op(Op) ) -> 
								(call(Operate) ->
									Res is 1
									;
									Res is 0
								)
	;
	Res is Operate	).


evaluteExp((Op,LeftNode,RightNode),Res) :- 
											
evaluteExp(LeftNode, ResLeft),  evaluteExp(RightNode,ResRight),
 Operate =.. [Op,ResLeft,ResRight] , 
		(is_logical_op(Op),is_one_or_zero(ResLeft,ResRight)) -> 
								(Op == /\  -> 
									Res is /\(ResLeft,ResRight)
									;
									Res is \/(ResLeft,ResRight)
								)
		;
		
((is_equality_op(Op); is_relat_op(Op)), call(Operate) -> Res is 1; Res is 0)
										;											
											Res is Operate.

 (assign(id(number),  ((OR),
							num(0), 	(AND), 
												((=<), num(0), num(0)), 	num(1))), []),
									

evaluteExp(Tree,Res) :- 
	%Tree = (O,LL,RR),write(LL),nl, write(RR),nl;
	(Tree = num(Res); (Tree = id(V), traceID(V,Res)));
(
	(Tree = (Op, num(X), num(Y))) ; 
	(Tree = (Op, id(V), num(Y)), traceID(V,X)) ;
	(Tree = (Op, num(X), id(V)), traceID(V,Y)); 
	(Tree = (Op, id(V1), id(V2)),  traceID(V1,X),traceID(V2,Y))),
	Operate =.. [Op,X,Y] ,
	(is_logical_op(Op), ((Op == /\ , Res is /\(X,Y))	; (Op == \/, Res is \/(X,Y))));
		
	((is_equality_op(Op); is_relat_op(Op) ), (call(Operate) ,Res is 1);	Res is 0);
	Res is Operate.


evaluteExp((Op,LeftNode,RightNode),Res) :- 
											
evaluteExp(LeftNode, ResLeft),  evaluteExp(RightNode,ResRight),
		Operate =.. [Op,ResLeft,ResRight] ,
		(is_logical_op(Op), (Op == /\ , Res is /\(ResLeft,ResRight))	;(Res is \/(ResLeft,ResRight)))	;
		  
		 ((is_equality_op(Op); is_relat_op(Op)), (call(Operate) , Res is 1) ; Res is 0)	;											
		Res is Operate.









































