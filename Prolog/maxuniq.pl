get_longest([], Max, Max).
get_longest([X|Xs], CurMax, Max) :-
	length(X, Len),
	NewMax is max(CurMax, Len),
	get_longest(Xs, NewMax, Max).

maxuniq(Lst, Res) :-
	findall(Spliting, valid_spliting(Lst, Spliting), AllSplitings),
	get_longest(AllSplitings, 0, Res).

valid_spliting(Lst, Spliting) :-
	gen_part(Lst, Spliting),
	is_set(Spliting).

gen_part([], []).
gen_part(Lst, [L1|Rest]) :-
	append(L1, L2, Lst),
	L1 \= [],
	gen_part(L2, Rest).
