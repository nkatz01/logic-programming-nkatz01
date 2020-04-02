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
tokenize(Str, Tokens,  [spaces(false), cased(true)]).

runDef(Tokens,Ast,Rest) :- my_tokenize('program factorial; begin read value; count := 1; result := 1; while count < value do begin count \n := count + 1; result := result * count end; write result end',Tokens) ,parse(Tokens, Ast,Rest).
%'number = 4 + 6 / 2 * 12 - 5 print lola'; 'number = 4 + 6 / 2 * 12 - 5 eggs = number';'number = 4 if (4 + 6 / 2 * 12 - 5 >= 35) print 1 else print 0 print nuchem'
%'dozen = 6 eggsneeded = 4 eggsbought = 6 sufficienteggs = eggsbought > dozen || eggsbought >= eggsneeded';'number = 1 || 1 && 1 && 0';'bool = 1 && 0 || 1 && 0';'number = 0 || 0 <= 0 && 1'
runDef1(Tokens,Ast,Rest) :- my_tokenize('number = 0 || 0 <= 0 && 1',Tokens) ,parse(Tokens, Ast,Rest).
%runPhar(Tokens,Ast,Rest) :- my_tokenize('(12+ 4)/6',Tokens),parse(Tokens, Ast,Rest) .

parse(Tokens, Ast,Rest) :-
  phrase(pl_program(Ast),Tokens,Rest),!.

 

pl_program(Ss)   --> rest_statements(Ss). %https://swish.swi-prolog.org/p/Compiler1.swinb


statement(assign(id(X), E)) --> identifier(X) ->  [punct(=)] -> cond_expre(E), {replace_existing_fact(id(X,_),id(X,E))}.
statement(if(T,S1,S2))     --> [word(if)], [punct('(')], cond_expre(T), [punct(')')], statement(S1), [word(else)], statement(S2).
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


pl_constant(N)     --> pl_integer(N), !. %Moved up with cut to avoid numbers appearing as name('1')
pl_constant(N)       --> identifier(X), {call(id(X,N))}.

pl_integer(X)              --> [number(X)].
identifier(X)              --> [word(X)].

%cond_expre(compare(Op, X, Y))    -->  relat_op(Op), expre(Y) ,.
%cond_expre(compare(Op, X, Y))    --> 	logical_op(Op), expre(Y) ,[punct(')')].


%https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm

cond_expre(T) -->  and_expre(E1), or_rest(E1,T). 	 



or_rest(E1,T) -->  [punct('|'),punct('|')],!, and_expre(E2),   {is_one_or_zero(E1,E2), V is \/(E1,E2)}, or_rest(V,T).
or_rest(T,T) --> [].

and_expre(T) --> equality_expre(E1), and_rest(E1,T).
and_rest(E1,T) --> [punct(&),punct(&)], !, equality_expre(E2), {is_one_or_zero(E1,E2),V is 	/\(E1,E2)}, and_rest(V,T).
and_rest(T,T) --> [].

equality_expre(T) -->   relat_expre(E1), equality_rest(E1,T).   
equality_rest(E1,T) --> equality_op(Op) ,!,  relat_expre(E2), {  Tr  =..[Op,E1,E2], (call(Tr) -> V is 1; V is 0)}, equality_rest(V,T).
equality_rest(T,T) --> [].

relat_expre(T) --> atomic_texpre(E1), relat_rest(E1,T).%could handle sperately equalitly Ops.%added cut
relat_rest(E1,T) -->  relat_op(Op) ,!, atomic_texpre(E2) , {Tr  =..[Op,E1,E2],( call(Tr) -> V is 1; V is 0 )},relat_rest(V,T).
relat_rest(T,T) --> [].

atomic_texpre(T) --> [punct('(')], !, cond_expre(T), [punct(')')];   	 arith_expre(T).

arith_expre(V) --> expre(V).

 
is_one_or_zero(E1,E2) :- (E1 == 1 ,E2 == 0);(E1 == 0, E2 == 1);(E1 == 1 ,E2 == 1);(E1 == 0, E2 == 0).
opening_paren('(') --> [punct('(')].
closing_paren(')') --> [punct(')')].

equality_op(==)         --> [punct(=),punct(=)].
equality_op(\=)        --> [punct(!),punct(=)].
%relat_op(==)         --> [punct(=),punct(=)].%applicable to numbers and bools
%relat_op(\=)        --> [punct(!),punct(=)].%applicable to numbers and bools




relat_op(>=)        --> [punct(>),punct(=)].%ONLY applicable to numbers
relat_op(>)         --> [punct(>)].%ONLY applicable to numbers

relat_op(=<)        -->  [punct(<),punct(=)].%ONLY applicable to numbers
relat_op(<)         --> [punct(<)].%ONLY applicable to numbers

%logical_op('||')        -->  [punct('|'),punct('|')].%NOT applicable to numbers
%logical_op('&&')        -->  [punct(&),punct(&)].%NOT applicable to numbers

%replace_each_existing_fact(OldVar, NewVar) :-
%forall(replace_existing_fact(OldVar, NewVar), true).

replace_existing_fact(OldVar, NewVar) :-
    (    retractall(OldVar)
    ->  assertz(NewVar);
       assertz(NewVar)
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


testPrint(Tokens,Ast) :- my_tokenize('print I will not laugh in class',Tokens),parse(Tokens, Ast).
