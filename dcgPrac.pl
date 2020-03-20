as --> [].
as --> [a], as.

tree_nodes(nil) --> [].
tree_nodes(node(Name, Left, Right)) -->
        tree_nodes(Left),
        [Name],
        tree_nodes(Right).

/*
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