sumThree(_, V, V, 3) :- !.

sumThree([_|Xs], V, A, C) :-
	sumThree(Xs, V, A, C).

sumThree([X|Xs], V, A, C) :-
	RES is A + X,
	CNT is C + 1,
	sumThree(Xs, V, RES, CNT).

sumThree(X, V) :- sumThree(X, V, 0, 0).
