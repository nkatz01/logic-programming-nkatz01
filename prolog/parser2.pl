% a grammar consisting of just letters “a” and “b” and counts number of a’s and b’s
% Note, SWI-Prolog must be run with the --traditional flag to work with the examples below.


s(CountA, CountB) --> "a", s(PriorCountA, CountB), { CountA is PriorCountA + 1 }.
s(CountA, CountB) --> "b", s(CountA, PriorCountB), { CountB is PriorCountB + 1 }.
s(0, 0) --> [].

% Try the following as queries:

% s(CountA, CountB, "aa", []).
% s(CountA, CountB, "aabba", []).

% requires the counts of a’s and b’s to be the same

s(CountAB) --> s(CountAB, CountAB).

% Try the following as queries:

% s(CountAB, "aabba", []).
% s(CountAB, "aabbab", []).
