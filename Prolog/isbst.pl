isLess(empty, _).
isLess(_, empty).
isLess(X, Y) :-  X \= empty, Y \= empty, X < Y.

isBst(empty, _, _).

isBst(btree(V, L ,R), MIN, MAX) :-
	isLess(V, MAX),
	isLess(MIN, V),
	isBst(L, MIN, V),
	isBst(R, V, MAX).

isBst(T) :- isBst(T, empty, empty).
