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