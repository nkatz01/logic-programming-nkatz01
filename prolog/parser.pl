% DCG rules for a single English sentence parser
% Note, SWI-Prolog must be run with the --traditional flag to work with the examples below.

sentence --> nounPhrase, " ", verbPhrase, ".".
nounPhrase --> determiner, " ", noun.
verbPhrase --> verb, " ", nounPhrase.
verbPhrase --> verb.
determiner --> "the".
determiner --> "a".
noun --> "cat".
noun --> "dog".
verb --> "chases".
verb --> "avoids".


% What valid sentences can we derive?

derive :-append("a cat avoids", _, S), sentence(S, []), printlist(S).

printlist([]) :- nl.
printlist([H|T]) :-
	put(H), printlist(T).
