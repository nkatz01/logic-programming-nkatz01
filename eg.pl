string_tokens(String, Tokens) :-
  string_chars(String, Chars),
  phrase(tokens(Tokens), Chars).
  
tokens([Token|Tokens]) --> ws, token(Token), ws, !, tokens(Tokens).
tokens([])             --> [].

ws --> [W], { char_type(W, space) }, ws.
ws --> [].

token(P) --> [C], { char_type(C, punct), \+char_type(C, quote), string_chars(P, [C]) }.
token(Q) --> quote(Cs), { string_chars(Q, Cs) }.
token(W) --> word(Cs), { string_chars(W, Cs) }.

quote([Quote|Ls])  --> [Quote], { char_type(Quote, quote) }, quote_rest(Quote, Ls).
quote_rest(Quote, [L|Ls]) --> [L], { L \= Quote }, quote_rest(Quote, Ls).
quote_rest(Quote, [Quote]) --> [Quote].

word([L|Ls])      --> [L], { char_type(L, alnum) }, word_rest(Ls).
word_rest([L|Ls]) --> [L], { char_type(L, alnum) }, word_rest(Ls).
word_rest([]) .   
