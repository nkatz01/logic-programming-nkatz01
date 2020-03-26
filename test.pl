:-  use_module(library(tokenize)).

my_tokenize(Str,Tokens):-
tokenize(Str, Tokens, [cased(true), spaces(false)]).

runDef(Tokens,Ast) :- my_tokenize('program factorial; begin read value; count := 1; result := 1; while count < value do begin count \n := count + 1; result := result * count end; write result end',Tokens) ,parse(Tokens, Ast).

runDef1(Tokens,Ast) :- my_tokenize('factorial',Tokens) ,parse(Tokens, Ast).
%runPhar(Tokens,Ast) :- my_tokenize('(12+ 4)/6',Tokens),parse(Tokens, Ast) .

parse(Tokens, Ast) :-
  pl_program(Ast,Tokens).

pl_program(['Program:'|S],Tokens) :- phrase(statement(S),Tokens),!.
 
/* 
pl_program(node(program, nill, node(S, nill, nill)))   :- phrase(statement(S),Tokens),!.
statement(node(S, nill, Ss))  --> statement(S),  rest_statements(Ss).
statement(node(assign, node(X, nill, nill), node(E, nill, nill))) --> [word(X)] ->  [punct(=)] -> [number(E)].
statement(node(print, nill, node(Ww, nill, nill)))  --> [word(print)], rest_statements(Ww).
statement([W|Ww]) --> [word(W)], rest_statements(Ww) .

rest_statements([])  --> [].
rest_statements(node(S, nill, Ss))    -->   statement(S), rest_statements(Ss).
 */


%statement([Ss])  -->   statement(Ss).%,  rest_statements(Ss).
%statement(['Assign:'|X]) --> [X] .
%statement(['print:'|[Ww]])  --> [word(print)], statement(Ww).
%statement([W|Ww]) --> [word(W)], statement(Ww).

statement([W|Ww]) -->  [word(W)],rest_statements(Ww).
statement([W|Ww]) -->  [punct(W)] ,rest_statements(Ww).
statement([W|Ww]) -->  [number(W)] ,rest_statements(Ww).
statement([W|Ww]) -->  [cntrl(W)] ,rest_statements(Ww).

rest_statements([])  --> [].
rest_statements([W|Ww])    -->   statement(W), rest_statements(Ww).



%----------------
%rest_statements([])  --> []. %[NotaWord], { NotaWord}.
%statement([S|Ss])  --> ([word(S)]; [punct(S)]; [number(S)]) ,statement(Ss).%,  rest_statements(Ss). 
testPrint(Tokens,Ast) :- my_tokenize('print I will not laugh in class',Tokens),parse(Tokens, Ast).


expression(X)              --> pl_constant(X).
expression(expr(Op, X, Y)) --> pl_constant(X), arithmetic_op(Op), expression(Y).

arithmetic_op("+")         --> ["+"].
arithmetic_op("-")         --> ["-"].
arithmetic_op("*")         --> ["*"].
arithmetic_op("/")         --> ["/"].

pl_constant(number(X))     --> pl_integer(X), !. % Moved up with cut to avoid numbers appearing as name('1')
pl_constant(name(X))       --> identifier(X).

pl_integer(X)              --> [number(X)].
identifier(X)              --> [word(X)].

test(compare(Op, X, Y))    --> expression(X), comparison_op(Op), expression(Y).

comparison_op("=")         --> ["="].
comparison_op("!=")        --> ["!","="].
comparison_op(">")         --> [">"].
comparison_op("<")         --> ["<"].
comparison_op(">=")        --> [">","="].
comparison_op("<=")        --> ["<","="].