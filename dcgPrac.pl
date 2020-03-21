%http://www.pathwayslms.com/swipltuts/dcg/
%https://www.metalevel.at/prolog/dcg
%http://kti.mff.cuni.cz/~bartak/prolog/lists.html
%https://ciao-lang.org/ciao/build/doc/ciao.html/dcg_doc.html
%https://github.com/iqhash/interpreters/blob/master/c-in-prolog/interpreter.pl
%https://github.com/triska/lisprolog
as --> [].
 
as --> [a]; [a], [b], as.
 
tree_nodes(nil) --> [].
tree_nodes(nil) --> [].
tree_nodes(node(Name, Left, Right)) -->
        tree_nodes(Left),
        [Name],
        tree_nodes(Right).


		
/*tree_nodes(node(Name, Left, Right)) -->
        tree_nodes(Left),
        [Name],
        tree_nodes(Right).


out_order(node(Name, Left, Right)) -->
        tree_nodes(Right),
        [Name],
        tree_nodes(Left).

		
		phrase(tree_nodes(node(a, node(b, nil,  node(c, nil, nil)), 	node(d, nil, nil))), Ns).
		phrase(tree_nodes(node(a, node(b, nil,  node(c, nil, nil)), 	node(d, nil, nil))), Ns).
		 
Ns = [b, c, a, d].
*/
% phrase(tree_nodes(node(/, node(+, 12,  2), 6, Ns).
% phrase(tree_nodes(node(+, 12, node(/, 4,  6), Ns).

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