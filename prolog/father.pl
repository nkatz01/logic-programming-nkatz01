father(a,b).  % 1
father(a,c).  % 2
father(b,d).  % 3
father(b,e).  % 4
father(c,f).  % 5

brother(Y,Z) :- father(X,Y),father(X,Z), not(Y = Z).

grandson(X,Y) :- father(Y,Z), father(Z,X).

descendent(X,Y) :- father(Y,X). % (1)
descendent(X,Y) :- father(Z,X), descendent(Z,Y). % (2)
