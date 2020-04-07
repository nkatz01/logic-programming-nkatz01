:- dynamic id/2.
:- use_module(library(tokenize)).
:- use_module(library(apply)).
:- use_module(library(pprint)).
:- use_module(library(lists)).
file('cmmExamples/example1.cmm').
file('cmmExamples/example2.cmm').
file('cmmExamples/example3.cmm').
file('cmmExamples/example4.cmm').
file('cmmExamples/example5.cmm').
my_tokenize_file :-   file(Link),    tokenize_file(Link, TokensContaminataed, [ cased(true)]),filterCtrlsAndDblSpaces(TokensContaminataed, Tokens), parse(Tokens, Ast,Rest) ,print(Rest), nl ,extractExp(Ast),nl ,fail.

are_identical(X, Y) :- %https://stackoverflow.com/questions/297996/prolog-filtering-a-list
    X == Y.

filterList(A, In, Out) :-
    exclude(are_identical(A), In, Out).


filterCtrlsAndDblSpaces(In, Out4) :- filterList(space(' '),In,Out1) , filterList(cntrl('\r'),Out1,Out2), filterList(cntrl('\n'),Out2,Out3),   filterList(cntrl('\t'),Out3,Out4).


 

my_tokenize(Str,Tokens):-
tokenize(Str, TokensContaminataed,  [cased(true)]), filterCtrlsAndDblSpaces(TokensContaminataed,Tokens).

%'number = 4 + 6 / 2 * 12 - 5 print number'; 'number = 4 + 6 / 2 * 12 - 5 eggs = number';'number = 4 if (4 + 6 / 2 * 12 - 5 >= 136) print 1 else print 0 print nuchem';'if (4 + 6 / 2 * 12 - 5 >= 35) print 1 else print 0 number = 4 print nuchem'
%'dozen = 6 eggsneeded = 4 eggsbought = 6 sufficienteggs = eggsbought > dozen || eggsbought >= eggsneeded';'number = 1 || 1 && 1 && 0';'bool = 1 && 0 || 1 && 0';'number = 0 || 0 <= 0 && 1';'while (3 <= 4) { f = 5 * 6}'
%'n = 500 low = 0 high = n + 1 while(high - low >= 2) { mid = (low + high) / 2 if (mid * mid <= n) low = mid else high = mid print low, high } print low'
runDef1(Tokens,Ast,Rest) :- my_tokenize('if (1 < 2) print 1 0',Tokens) ,parse(Tokens, Ast,Rest)  , extractExp(Ast)   .%, retractall(id(_,_))
%runPhar(Tokens,Ast,Rest) :- my_tokenize('(12+ 4)/6',Tokens),parse(Tokens, Ast,Rest) .
%'if (1 < 2) print 1 else print 0 if (1 > 2) print 0 else print 1''divisor = 2 number = 4 + 6 / divisor * 12 - 5';'number = 4 + 6 / 2 * 12 - 5 eggs = number addingthem = eggs + number + 4 doubleit = addingthem * 2'
parse(Tokens, Ast,Rest) :-
  phrase(pl_program(Ast),Tokens,Rest),!.
 
pl_program(Ss)   --> rest_statements(Ss). %https://swish.swi-prolog.org/p/Compiler1.swinb


statement(assign(id(X), E)) --> identifier(X), {X \= 'print', X \= 'if', X \= 'while'},    [punct(=)] -> cond_expre(E), {  assertThisFact(id(X,_)) }.


statement(if(T,(S1,[]),(S2,[])))     --> [word(if)], [punct('(')], cond_expre(T), [punct(')')], statement(S1), [word(else)], statement(S2).

%statement(if(T,(S,[])))     --> [word(if)], [punct('(')], cond_expre(T), [punct(')')], statement(S).%, {\+(S == print(statement(S)); S == while(T,S))}


statement(while(T,S))  --> [word('while')] -> [punct('(')] -> cond_expre(T) -> [punct(')')] -> [punct('{')], rest_statements(S), [punct('}')]  .
statement(print(statements(S)))  --> [word(print)] -> statement(W),  {flatten(W,S)}.						 	
statement([W|Ww]) --> pl_constant(W) ->  [punct(,)] , statement(Ww). 
statement(W) --> pl_constant(W),  statement. %{W \= 'while'}, 
statement --> [].



rest_statements((S, Ss))    -->   statement(S), rest_statements(Ss).
rest_statements([])  --> [].



pl_constant(num(N))     --> pl_integer(N), !. %Moved up with cut to avoid numbers appearing as name('1')
pl_constant(id(X))       --> identifier(X), {call(id(X,_)) /*; write('Sorry, you\'re using unknown variable'),nl)*/}.

pl_integer(X)              --> [number(X)].
identifier(X)              --> [word(X)].

assertThisFact(Fact):- %https://stackoverflow.com/questions/10437395/prolog-how-to-assert-make-a-database-only-once
    \+( Fact ),!,         % \+ is a NOT operator.
    assert(Fact).
assertThisFact(_).


replace_each_existing_fact(OldVar, NewVar) :-
forall(replace_existing_fact(OldVar, NewVar), true).

replace_existing_fact(OldVar, NewVar) :-
       call(OldVar) ,
	retract(OldVar),
   assert(NewVar)
   
     .%https://stackoverflow.com/questions/37871775/prolog-replace-fact-using-fact
 
%\+var(Results),write(Results), tab(1),write('is gen'), nl,	 
extractExp([]). 
extractExp((LeftNode,RightTree)) :-  								 
	(LeftNode = assign(id(Id),E),	evaluteExp(E,Results1), 	Results is floor(Results1),    replace_each_existing_fact(id(Id,_),id(Id,num(Results))), extractExp(RightTree)),!;
	(LeftNode = print(statements(List)) , printStatements(List),  extractExp(RightTree)),!; 
	 (LeftNode = if(T,S1,S2), evaluteExp(T,Results) , ((Results == 1, extractExp(S1));	(Results == 0, extractExp(S2))), extractExp(RightTree)),!;
	 (LeftNode = while(T,S), evaluteExp(T,Results),  (	(Results == 1, extractExp(S),  extractExp((LeftNode,RightTree)))	; ( extractExp(RightTree) ) )	)	.%	,!;
	%extractExp(RightTree).


evaluteExp(Tree,Res) :- 
 	Tree = num(Res); (Tree = id(V), traceID(V,Res));
(	(Tree = (Op, num(X), num(Y))) ; 	(Tree = (Op, id(V), num(Y)), traceID(V,X)) ;	(Tree = (Op, num(X), id(V)), traceID(V,Y)); 	(Tree = (Op, id(V1), id(V2)),  traceID(V1,X),traceID(V2,Y))	),
	
	( applyRelational(Op,X,Y,Res);applyLogical(Op,X,Y,Res); applyArith(Op,X,Y,Res))	.


evaluteExp((Op,LeftNode,RightNode),Res) :- 
											
evaluteExp(LeftNode, ResLeft),  evaluteExp(RightNode,ResRight),

( applyRelational(Op,ResLeft,ResRight,Res); applyLogical(Op,ResLeft,ResRight,Res);applyArith(Op,ResLeft,ResRight,Res)).


applyRelational(Op,X,Y,Res) :- (is_equality_op(Op); is_relat_op(Op)), Operate =.. [Op,X,Y] ,	((call(Operate),		Res is 1)	;	Res is 0).
								
applyArith(Op,X,Y,Res) :-  \+is_logical_op(Op), Operate =.. [Op,X,Y] , Res is Operate .


applyLogical(Op,X,Y,Res) :-    	 is_logical_op(Op), (is_one_or_zero(X,Y);	(write('Only 1 and 0 can be AND\\ORD'),nl,false)),	((Op == /\  ,		Res is /\(X,Y))		;	(Op == \/ , 	Res is \/(X,Y)	)).	% ; write('Only 1 and 0 can be AND\\ORD'),nl




printStatements([]) :- nl, !.%https://stackoverflow.com/questions/22646284/how-can-i-print-items-in-a-list-one-by-one
printStatements([Head|Tail]) :-
(Head = num(Res); (Head = id(V), traceID(V,Res))),
   print(Res),
   tab(1),
   printStatements(Tail).
   
   
/*
applyLogical(Op,X,Y,Res) :- is_one_or_zero(X,Y), 
								(Op == /\  -> 
									Res is /\(X,Y)
									;
									Res is \/(X,Y)
								).


applyLogical(Op,X,Y,Res) :- ((is_one_or_zero(X,Y) ,(Op == /\ )  ->  Res is /\(X,Y)
																; 
																((Op == \/ ,is_one_or_zero(X,Y) -> Res is \/(X,Y) ; fail)
																; fail).*/
								
traceID(Id,FinalNum) :-  Id = num(FinalNum).
traceID(Id,LinkIdOrEnd) :- call(id(Id,IntermediateLink)), traceID(IntermediateLink,LinkIdOrEnd).

/*
%callExtract(ProgAst,InnerAsts,ConvertedToLsExprs):- ProgAst =.. [H|InnerAsts],  extractTree(InnerAsts,ConvertedToLsExprs).

extractTree([],[]). 
extractTree((LeftNode,RightTree)) :- 

LeftNode = while(T,S), (
	call(id(Predicate,Y)),
  ( Y = 1, !
   ;
   retract(id(X,Y),
   assertz(id(X,NewY),

) ,!; .
extractTree((LeftNode,RightTree)) 

processNode((LeftNode,RightNode)) :-



%LeftNode = if(T,S1,S2), ,!; .




dountilstop(Predicate,NewY) :-
  repeat,
	call(id(Predicate,Y)),
  ( Y = 1, !
   ;
   retract(id(X,Y),
   assertz(id(X,NewY),
   fail
  ).
*/


cond_expre(T) -->  and_expre(E1), or_rest(E1,T). 	 

or_rest(E1,T) -->  [punct('|'),punct('|')],!, and_expre(E2),   {V  = (\/,E1,E2)}, or_rest(V,T).%
or_rest(T,T) --> [].

and_expre(T) --> equality_expre(E1), and_rest(E1,T).
and_rest(E1,T) --> [punct(&),punct(&)], !, equality_expre(E2), {V  = (/\,E1,E2)}, and_rest(V,T).%
and_rest(T,T) --> [].

equality_expre(T) -->   relat_expre(E1), equality_rest(E1,T).   
equality_rest(E1,T) --> equality_op(Op) ,!,  relat_expre(E2), {  V=(Op,E1,E2)}, equality_rest(V,T).
equality_rest(T,T) --> [].

relat_expre(T) --> atomic_texpre(E1), relat_rest(E1,T).%could handle sperately equalitly Ops.
relat_rest(E1,T) -->  relat_op(Op) ,!, atomic_texpre(E2) , { V=(Op,E1,E2) },relat_rest(V,T).
relat_rest(T,T) --> [].

atomic_texpre(T) -->  arith_expre(T); [punct('(')], !, cond_expre(T), [punct(')')]    	.
arith_expre(V) --> expre(V).

is_one_or_zero(E1,E2) :- (E1 == 1 ,E2 == 0);(E1 == 0, E2 == 1);(E1 == 1 ,E2 == 1);(E1 == 0, E2 == 0).
opening_paren('(') --> [punct('(')].
closing_paren(')') --> [punct(')')].

equality_op(==)         --> [punct(=),punct(=)].%applicable to numbers and bools
equality_op(\=)        --> [punct(!),punct(=)].%applicable to numbers and bools
relat_op(>=)        --> [punct(>),punct(=)].%ONLY applicable to numbers
relat_op(>)         --> [punct(>)].%ONLY applicable to numbers
relat_op('=<')        -->  [punct(<),punct(=)].%ONLY applicable to numbers
relat_op(<)         --> [punct(<)].%ONLY applicable to numbers

is_equality_op((Op)) :- Op == '==' ; Op == '\\='  .
is_relat_op((Op)):- Op == '>=' ; Op == '=<' ; Op == '>' ; Op == '<'.
is_logical_op((Op)):- (Op == /\) ; (Op == \/).

expre(N) --> multiplicative(N1), additive_rest(N1,N).%https://stackoverflow.com/questions/7543100/grammar-involving-braces

additive_rest(N1,N) --> [punct('+')], !, multiplicative(N2), {N3 = (+,N1,N2)}, additive_rest(N3,N);  
						[punct('-')], !, multiplicative(N2), {N3 = (-,N1,N2)}, additive_rest(N3,N).
additive_rest(N,N) --> [].

multiplicative(N) --> atomic(N1), multiplicative_rest(N1,N).
multiplicative_rest(N1,N) --> [punct('*')], !, atomic(N2), {N3 = (*,N1,N2)}, multiplicative_rest(N3,N);
								[punct('/')], !, atomic(N2), {N3 = (/,N1,N2)}, multiplicative_rest(N3,N);	
								[punct('%')], !, atomic(N2), {N3 = (mod,N1,N2)}, multiplicative_rest(N3,N).
multiplicative_rest(N,N) --> [].

atomic(N) --> [punct('(')], !, expre(N), [punct(')')];  num(N). 
num(N) --> pl_constant(N).


%logical_op('||')        -->  [punct('|'),punct('|')].%NOT applicable to numbers
%logical_op('&&')        -->  [punct(&),punct(&)].%NOT applicable to numbers





/*
runEval(Tokens,Ast,RestOfTokens,ExprLs,Results,RestOfEval) :- runDef1(Tokens,Ast,RestOfTokens) ,
myeval(Ast,NestLs),
flatten(NestLs,ExprLs),  
phrase(expre(Results), ExprLs,RestOfEval).

myeval(expr(Op,number(N1),number(N2)),[N1,Op, N2]).
myeval(expr(Op,number(N),Expr),Ans) :-  Ans = [N,Op, Ls], myeval(Expr,Ls).
*/


