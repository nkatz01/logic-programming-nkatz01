:-  use_module(library(tokenize)).
% {write('im called')} ,
% {format('X=~w~n', [X])},
my_tokenize1(Tokens) :- tokenize('n = 8
k = 1
f = 1
while (k <= n) {
f = f * k
print k,f
k = k + 1
}',Tokens,[cased(true), spaces(false)]).

my_tokenize(Str,Tokens):-
tokenize(Str, Tokens, [cased(true), spaces(false)]).

runDef(Tokens,Ast,Rest) :- my_tokenize('program factorial; begin read value; count := 1; result := 1; while count < value do begin count \n := count + 1; result := result * count end; write result end',Tokens) ,parse(Tokens, Ast,Rest).
%'number = 4 + 6 / 2 * 12 - 5 print lola'
runDef1(Tokens,Ast,Rest) :- my_tokenize('number = 4 + 6 / 2 * 12 - 5 eggs = number',Tokens) ,parse(Tokens, Ast,Rest).
%runPhar(Tokens,Ast,Rest) :- my_tokenize('(12+ 4)/6',Tokens),parse(Tokens, Ast,Rest) .

parse(Tokens, Ast,Rest) :-
  phrase(pl_program(Ast),Tokens,Rest),!.

 

pl_program(Ss)   --> rest_statements(Ss).


statement(assign(id(X), E)) --> identifier(X) ->  [punct(=)] -> expre(E), {replace_existing_fact(id(X,_),id(X,E))}.

statement(print(statements([W])))  --> [word(print)] -> statement(W).						 	
statement([W|Ww]) --> [word(W)] ->  [punct(,)] , statement(Ww). 
statement([N|Ww]) --> [number(N)] -> [punct(,)]    , statement(Ww).
statement(W) --> [word(W)], statement. 
statement(N) --> [number(N)], statement. 
statement --> [].

%statement([W|Ww]),['}']  --> [punct(,)] -> [word(W)] -> [punct('}')],rest_statements(Ww).
%statement([W|Ww]),[X]  --> [punct(,)] ,rest_statements(Ww).%-> [word(X)]


rest_statements((S, Ss))    -->   statement(S), rest_statements(Ss).
rest_statements([])  --> [].




arithmetic_op(+)         --> [punct(+)].
arithmetic_op(-)         --> [punct(-)].
arithmetic_op(*)         --> [punct(*)].
arithmetic_op(/)         --> [punct(/)].
arithmetic_op(mod)         --> [punct('%')].

pl_constant(N)     --> pl_integer(N), !. % Moved up with cut to avoid numbers appearing as name('1')
pl_constant(N)       --> identifier(X), {call(id(X,N))}.

pl_integer(X)              --> [number(X)].
identifier(X)              --> [word(X)].

test(compare(Op, X, Y))    --> expression(X), comparison_op(Op), expression(Y).

comparison_op("=")         --> ["="].
comparison_op("!=")        --> ["!","="].
comparison_op(">")         --> [">"].
comparison_op("<")         --> ["<"].
comparison_op(">=")        --> [">","="].
comparison_op("<=")        --> ["<","="].

replace_each_existing_fact(OldVar, NewVar) :-
    forall(replace_existing_fact(OldVar, NewVar), true).
replace_existing_fact(OldVar, NewVar) :-
    (   call(OldVar)
    ->  retract(OldVar),
        assertz(NewVar)
    ;   assertz(NewVar)
    ).%https://stackoverflow.com/questions/37871775/prolog-replace-fact-using-fact



runEval(Tokens,Ast,RestOfTokens,ExprLs,Results,RestOfEval) :- runDef1(Tokens,Ast,RestOfTokens) ,
myeval(Ast,NestLs),
flatten(NestLs,ExprLs),  
phrase(expre(Results), ExprLs,RestOfEval).



 
myeval(expr(Op,number(N1),number(N2)),[N1,Op, N2]).
myeval(expr(Op,number(N),Expr),Ans) :-  Ans = [N,Op, Ls], myeval(Expr,Ls).

%callExtract(ProgAst,InnerAsts,ConvertedToLsExprs):- ProgAst =.. [H|InnerAsts],  extractExp(InnerAsts,ConvertedToLsExprs).

extractExp([],[]). 
extractExp((LeftNode,RightTree),ConvertedToLsExprs) :- LeftNode = assign(_,E), myeval(E,ConvertedToLsExprs),!;   extractExp(RightTree,ConvertedToLsExprs).


expre(N) --> multiplicative(N1), additive_rest(N1,N).%https://stackoverflow.com/questions/7543100/grammar-involving-braces
additive_rest(N1,N) --> [punct(+)], !, multiplicative(N2), {N3 is N1+N2}, additive_rest(N3,N);   [punct(-)], !, multiplicative(N2), {N3 is N1-N2}, additive_rest(N3,N).
additive_rest(N,N) --> [].
multiplicative(N) --> atomic(N1), multiplicative_rest(N1,N).
multiplicative_rest(N1,N) --> [punct(*)], !, atomic(N2), {N3 is N1*N2}, multiplicative_rest(N3,N);	[punct(/)], !, atomic(N2), {N3 is N1/N2}, multiplicative_rest(N3,N);	[punct('%')], !, atomic(N2), {N3 is mod(N1,N2)}, multiplicative_rest(N3,N).
multiplicative_rest(N,N) --> [].
atomic(N) --> [punct('(')], !, expre(N), [punct(')')];  num(N). 
num(N) --> pl_constant(N).


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
testPrint(Tokens,Ast) :- my_tokenize('print I will not laugh in class',Tokens),parse(Tokens, Ast).
