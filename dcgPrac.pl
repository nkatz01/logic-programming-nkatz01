:- use_module(library(tokenize)).
%use_module(library(quintus)).
%http://www.pathwayslms.com/swipltuts/dcg/
%https://www.metalevel.at/prolog/dcg
%http://kti.mff.cuni.cz/~bartak/prolog/lists.html
%https://ciao-lang.org/ciao/build/doc/ciao.html/dcg_doc.html
%https://github.com/iqhash/interpreters/blob/master/c-in-prolog/interpreter.pl
%https://github.com/triska/lisprolog

   
   use_module(library(tokenize)).
:- use_module(library(dcg/basics), [eos//0, number//1]).
as --> [].
 
as --> [a]; [a], [b], as.
 
out_order(nil) --> [].
out_order(nil) --> [].
out_order(node(Name, Left, Right)) --> %use this for 12+4/6. appends to list in out-order, therefor, exectuion should begin backwards - frm end of list to begining
        out_order(Right),
        [Name],
        out_order(Left).

in_order(nil) --> [].		
in_order(node(Name, Left, Right)) --> %%(12+ 4)/6
        in_order(Left),
        [Name],
        in_order(Right).	
		
% phrase(out_order(node('+', node('/', node(6, nil, nil),  node(4, nil, nil)),node(12, nil, nil))), Ns). %12+ 4/6

% phrase(in_order(node('/',  node(6, nil, nil),node('+', node(12, nil, nil),  node(4, nil, nil)))), Ns). %(12+ 4)/6
%alternatively, change around the children trees.
% phrase(out_order(node('/', node('+', node(4, nil, nil), node(12, nil, nil)), node(6, nil, nil))), Ns).

tree_nodes(nil) --> [].
tree_nodes(node(Name, Left, Right)) -->
        tree_nodes(Left),
        [Name],
        tree_nodes(Right).
		


sentence(S) :-
  nounphrase(S-S1),
  verbphrase(S1-[]).

 

nounphrase(NP-X):-
  determiner(NP-S1),
  nounexpression(S1-X).
nounphrase(NP-X):-
  nounexpression(NP-X).

nounexpression(NE-X):-
  noun(NE-X).
  
nounexpression(NE-X):-
  adjective(NE-S1),
  nounexpression(S1-X).

verbphrase(VP-X):-
  verb(VP-S1),
  nounphrase(S1-X).

determiner([the|X]-X).
determiner([a|X]-X).  
noun([dog|X]-X).
noun([cat|X]-X).
noun([mouse|X]-X).

verb([ate|X]-X).
verb([chases|X]-X).

adjective([big|X]-X).
adjective([brown|X]-X).
adjective([lazy|X]-X).

command(OutputList, InputList).
command([V], InList):- verb(V, InList-[]).

command([V,O], InList) :-
  verb(Object_Type, V, InList-S1),
  object(Object_Type, O, S1-[]).
  
verb(look, [look|X]-X).% :-  format('X=~w~n', [X]).
verb(look, [look,around|X]-X).
verb(list_possessions, [inventory|X]-X).
verb(end, [end|X]-X).
verb(end, [quit|X]-X).
verb(end, [good,bye|X]-X).

verb(place, goto, [X|Y]-[X|Y]):- room(X).
verb(place, goto, [dining,room|Y]-[dining,room|Y]).

verb(place, goto, [go,to|X]-X).
verb(place, goto, [go|X]-X).
verb(place, goto, [move,to|X]-X).

verb(thing, take, [take|X]-X).
verb(thing, drop, [drop|X]-X).
verb(thing, drop, [put|X]-X).
verb(thing, turn_on, [turn,on|X]-X).

object(Type, N, S1-S3) :-
  det(S1-S2),
  noun(Type, N, S2-S3).
object(Type, N, S1-S2) :-
  noun(Type, N, S1-S2).


  
noun(place, R, [R|X]-X):- room(R).
noun(place, 'dining room', [dining,room|X]-X).

noun(thing, T, [T|_]-X):- location(T,_).%try  command(X,[drop,at,office,space]).
noun(thing, T, [T|X]-X):- have(T).

noun(thing, light, [light|X]-X).
noun(thing, 'washing machine', [washing,machine|X]-X).
noun(thing, flashlight, [light|X]-X):- have(flashlight).

det([the|X]- X).
det([a|X]-X).
det([an|X]-X).
det([at|X]-X).
det([to|X]-X).

room(X):- X = office.
room(X):- X = 'dining room'.
room(kitchen).
%location(T,_) :- T = office.
location(office,_).
have(T) :- T = apple.

 
 
 
 

ope(42,X,Y,R) :- R is X * Y.



/*
sentence --> nounphrase, verbphrase.
nounphrase --> determiner, nounexpression.
nounphrase --> nounexpression.
nounexpression --> noun.
nounexpression --> adjective, nounexpression.

verbphrase --> verb, nounphrase.

determiner --> the ; a.
noun --> dog ; bone ; mouse ; cat.
verb --> ate ; chases.
adjective --> big ; brown ; lazy.			
 


lower_case --> 
	[Letter],
	{	Letter @>='a',
		Letter @=<'z'}.
term(atom) -->
	lower_case, remaining_terms.
	
remaining_terms --> 
	(lower_case;
	upper_case;
	under_score;
	digit),
	remaining_terms.
	
remaining_terms -->
	[].
*/
%https://vimeo.com/53104831	

cliche -->
    thing,
    " is a ",
    type_of_thing,
    " trapped in a ",
    opposite_type_of_thing,
    " body.".
thing --> "Cygwin".
type_of_thing --> "Unix OS".
opposite_type_of_thing --> "Windows'".

%check_arg(arg) --> 


 fizz_buzz(Msg) --> anything, fizz(Msg), anything, buzz, anything.
anything --> [].
anything --> [_], anything.
fizz(Msg) -->
   "fizz",
    {
        format('At fizz we have Msg=~w~n', [Msg])
    }.
buzz -->
    "buzz".

	
%	Grammar rules.

operator("+")--> ["+"].
operator("-")--> ["-"].
operator("*")--> ["*"].
operator("/")--> ["/"].


 