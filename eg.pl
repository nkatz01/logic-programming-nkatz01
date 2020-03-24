

%   male(Person).
%   This Person is male.

male(alan).
male(bruce).
male(clive).
male(dave).
male(eddie).
male(fred).
male(greg).
male(henry).


%   female(Person).
%   This Person is female.

female(andrea).
female(betty).
female(clarissa).
female(doris).
female(elsie).
female(freda).
female(greta).


%   married(Person1, Person2).
%   Person1 is married to Person2.

married(alan, andrea).
married(bruce, betty).
married(clive, clarissa).
married(eddie, elsie).
married(fred, freda).
married(greg, greta).

%   PROBLEM 1
%   How do you find out if someone is the ancestor of someone else ?
parent(alan, clive).
parent(andrea, clive).
parent(bruce, clarissa).
parent(betty, clarissa).
parent(clive, dave).
parent(clarissa, dave).
parent(clive, doris).
parent(clarissa, doris).
parent(eddie, greg).
parent(elsie, greg).
parent(fred, greta).
parent(freda, greta).
parent(greg, henry).
parent(greta, henry).


ancestor(A, B):-	% A is B's ancestor if
    parent(A, B).	% A is B's parent.

ancestor(A, B):-	% A is B's ancestor if
    parent(P, B),	% some person P is B's parent and
    ancestor(A, P). 	% A is P's ancestor.

