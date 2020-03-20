  
copyfile(Infile, Outfile) :-
 see(Infile), tell(Outfile), copy, seen, told.

copy :- get_char(C), loop(C).

loop(end_of_file) :- !.
loop(C) :- put_char(C), copy.

main :- copyfile('input.txt', 'output.txt').