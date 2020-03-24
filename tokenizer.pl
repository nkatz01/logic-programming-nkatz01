tokenize(`1 * 3`, Tokens), untokenize(Tokens, Text), Tokens = [A,_,B,_,C|T].
tokenize(`1 * 3`, Tokens), untokenize(Tokens, Text), Tokens = [A,_,B,_,C],A = number(X), C = number(Z), B=punct((Y)),Ans is  X * Z.
 tokenize(`1 * 3`, Tokens), untokenize(Tokens, Text), Tokens = [A,_,B,_,C],A = number(X), C = number(Z), untokenize([B],Y),Y = [H], char_code(M,H), Ans is  X H Z.	
 
  tokenize(`2 * 3`, Tokens), untokenize(Tokens, Text), Tokens = [A,_,B,_,C],A = number(X), C = number(Z), untokenize([B],[Y]), op