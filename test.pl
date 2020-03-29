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

runDef(Tokens,Ast) :- my_tokenize('program factorial; begin read value; count := 1; result := 1; while count < value do begin count \n := count + 1; result := result * count end; write result end',Tokens) ,parse(Tokens, Ast,Rest).

runDef1(Tokens,Ast,Rest) :- my_tokenize('number = 12 / 4 + 6',Tokens) ,parse(Tokens, Ast,Rest).
%runPhar(Tokens,Ast,Rest) :- my_tokenize('(12+ 4)/6',Tokens),parse(Tokens, Ast,Rest) .

parse(Tokens, Ast,Rest) :-
  pl_program(Ast,Tokens,Rest).

%pl_program(['Program:'|S],Tokens) :- phrase(rest_statements(S),Tokens,Rest),!.%used together with the lists version
 

pl_program(program(S),Tokens,Rest)   :- phrase(rest_statements(S),Tokens,Rest),!.


statement(assign(id(X), E)) --> identifier(X) ->  [punct(=)] -> expression(E).
statement(print(statements([W])))  --> [word(print)] -> statement(W).						 	
statement([W|Ww]) --> [word(W)] ->  [punct(,)] , statement(Ww). 
statement([N|Ww]) --> [number(N)] -> [punct(,)]    , statement(Ww).
statement(W) --> [word(W)].
statement(N) --> [number(N)]. 

%statement([W|Ww]),['}']  --> [punct(,)] -> [word(W)] -> [punct('}')],rest_statements(Ww).
 

%statement([W|Ww]),[X]  --> [punct(,)] ,rest_statements(Ww).%-> [word(X)]


rest_statements((S, Ss))    -->   statement(S), rest_statements(Ss).
rest_statements([])  --> [].


expression(expr(O2,expr(O, X, Z),Y)) --> pl_constant(X), arithmetic_op(Op),  {Op = (O,4)}, pl_constant(Z),  arithmetic_op(Op2) ,{Op2 = (O2,Whatever)} , expression(Y).

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



pl_constant(number(X))     --> pl_integer(X), !. % Moved up with cut to avoid numbers appearing as name('1')
pl_constant(id(X))       --> identifier(X).

pl_integer(X)              --> [number(X)].
identifier(X)              --> [word(X)].

test(compare(Op, X, Y))    --> expression(X), comparison_op(Op), expression(Y).

comparison_op("=")         --> ["="].
comparison_op("!=")        --> ["!","="].
comparison_op(">")         --> [">"].
comparison_op("<")         --> ["<"].
comparison_op(">=")        --> [">","="].
comparison_op("<=")        --> ["<","="].
/*  


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
