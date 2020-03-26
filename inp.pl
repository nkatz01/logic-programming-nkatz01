:-  use_module(library(tokenize)).

%tokenize_file('/mmExamples/example1.cmm', Tokens, [cased(true), spaces(false)]).
my_tokenize(Str,Tokens):-
tokenize(Str, Tokens, [cased(true), spaces(false)]).

 runDef(Tokens,Ast) :- my_tokenize('12+ 4/6',Tokens),parse(Tokens, Ast)
 runPhar(Tokens,Ast) :- my_tokenize('(12+ 4)/6',Tokens),parse(Tokens, Ast) .

%runDef(Tokens,Ast) :- my_tokenize('12+ 4/6',Tokens),clean(Tokens, CTokens)
%runPhar(Tokens,Ast) :- my_tokenize('(12+ 4)/6',Tokens),clean(Tokens, CTokens) .


parse(Tokens, Ast) :-
  phrase(pl_program(Ast), Tokens).

pl_program(S)              --> statement(S).

statement((S;Ss))          --> statement(S), rest_statements(Ss).
statement(assign(X,E))     --> identifier(X), [":", "="], expression(E).
statement(if(T,S1,S2))     --> ["if"], test(T), ["then"], statement(S1), ["else"], statement(S2).
statement(while(T,S))      --> ["while"], test(T), ["do"], statement(S).
statement(pl_read(X))      --> ["read"], identifier(X).
statement(pl_write(X))     --> [word(print)], expression(X).

rest_statements((S;Ss))    --> [";"], statement(S), rest_statements(Ss).
rest_statements(void)      --> ["end"].

expression(X)              --> pl_constant(X).
expression(expr(Op, X, Y)) --> pl_constant(X), arithmetic_op(Op), expression(Y).

arithmetic_op("+")         --> ["+"].
arithmetic_op("-")         --> ["-"].
arithmetic_op("*")         --> ["*"].
arithmetic_op("/")         --> ["/"].

pl_constant(number(X))     --> pl_integer(X), !. % Moved up with cut to avoid numbers appearing as name('1')
pl_constant(name(X))       --> identifier(X).

pl_integer(X)              --> [Y], { number_string(X, Y) }.
identifier(X)              --> [Y], { atom_string(X, Y) }.

test(compare(Op, X, Y))    --> expression(X), comparison_op(Op), expression(Y).

comparison_op("=")         --> ["="].
comparison_op("!=")        --> ["!","="].
comparison_op(">")         --> [">"].
comparison_op("<")         --> ["<"].
comparison_op(">=")        --> [">","="].
comparison_op("<=")        --> ["<","="].
/*





*/