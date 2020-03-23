use_module(library(quintus)).
%http://www.pathwayslms.com/swipltuts/dcg/
%https://www.metalevel.at/prolog/dcg
%http://kti.mff.cuni.cz/~bartak/prolog/lists.html
%https://ciao-lang.org/ciao/build/doc/ciao.html/dcg_doc.html
%https://github.com/iqhash/interpreters/blob/master/c-in-prolog/interpreter.pl
%https://github.com/triska/lisprolog
as --> [].
 
as --> [a]; [a], [b], as.
 
out_order(nil) --> [].
tree_nodes(nil) --> [].
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

/*
		
 


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


 