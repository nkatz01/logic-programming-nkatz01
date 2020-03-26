:-  use_module(library(tokenize)).

my_tokenize(Str,Tokens):-
tokenize(Str, Tokens, [cased(true), spaces(false)]).

 runDef(Tokens,Ast) :- my_tokenize('print I will not laugh in class',Tokens),parse(Tokens, Ast).
% runPhar(Tokens,Ast) :- my_tokenize('(12+ 4)/6',Tokens),parse(Tokens, Ast) .

parse(Tokens, Ast) :-
  phrase(statement(Ast), Tokens).

 
statement(node(print, nill, node(Ww, nill, nill)))  --> [word(print)], rest_statements(Ww).
 

rest_statements([W|Ww])    --> [word(W)], rest_statements(Ww).
rest_statements([])  --> []. %[NotaWord], { NotaWord}. 





testPrint(Tokens,Ast) :- my_tokenize('print I will not laugh in class',Tokens),parse(Tokens, Ast).
