is_bigger(cat, mouse).

jesvisla(bod(X, _), bod(X, _)).

delka(bod(Ax, Ay), bod(Bx, By), R) :- R is sqrt( (Ax - Bx)*(Ax - Bx) + (Ay - By)*(Ay - By)).

fact(0, 1).
fact(N,R) :-
	N > 0,
	NewN is N - 1,
	fact(NewN, R1),
	R is N * R1.

fact2(0, Acc, Acc).
fact2(N, Acc, R) :-
	N > 0,
	NewAcc is N * Acc,
	NewN is N - 1,
	fact2(NewN, NewAcc, R).

fact1(N, R) :- fact2(N, 1, R).

fib(0, 0).
fib(1, 1).

fib(N, R) :- 
	N > 1,
	Prev is N - 1,
	PrevPrev is N -2,
	fib(Prev, R1),
	fib(PrevPrev, R2),
	R is R1 + R2.

puzzle(N) :-
    Digits = [0,1,2,3,4,5,6,7,8,9],
    member(A, Digits), A \= 0,
    member(B, Digits),
    member(C, Digits),
    member(D, Digits),
    member(E, Digits),


    E =:= A + B + C,
    E - 4 =:= C,
    E - 1 =:= (A + D),
    E =:= C * 3,
    N is A*10000 + B*1000 + C*100 + D*10 + E.


    my_append([], Elem, [Elem]).
    my_append([X|Xs], Elem, [X|Res]) :- my_append(Xs, Elem, Res).

    contains([X|_], X).
    contains([X|Xs], E) :- X \= E, contains(Xs, E).

    not_contains(Lst, E) :- contains(Lst, E), !, fail.
    not_contains(_, _).

    nth([X|_], 0, X).
    nth([_|Xs], N, R) :- 
	N > 0,
	NewN is N - 1,
	nth(Xs, NewN, R).

    length([], Cnt, Cnt).
    length([_|Xs], Cnt, Res) :- NewCnt is Cnt + 1, length(Xs, NewCnt, Res).

    length1(Lst, R) :- length(Lst, 0, R).


    lengthwithSub([], 0):- !.

    lengthwithSub([X|Xs], Res) :- 
	    is_list(X),
	    lengthwithSub(X, R1),
	    lengthwithSub(Xs, R2),
	    Res is R1 + R2, !.

    lengthwithSub([_|Xs], Res) :- lengthwithSub(Xs, R1), Res is R1 + 1.

    prepend(Lst, El, [El|Lst]).

    % delete jsut firts ocurance 
    delete([], _, []).
    delete([El|Xs], El, Xs) :- !.
    delete([X|Xs], El, [X|Res]) :- delete(Xs, El, Res).

    % delete all ocurences - pokud je result moc dlouhej da to ...
    delete2([], _, []).
    delete2([El|Xs], El, Res) :- delete2(Xs, El, Res), !.
    delete2([X|Xs], El, [X|Res]) :- X \= El, delete2(Xs, El, Res).

    replace([], _, _, []).
    replace([X|Xs], X, R, [R|Xs]).
    replace([X|Xs], S, R, [X|Res]) :- X \= S, replace(Xs, S, R, Res).

    myLast([X], X) :- !.
    myLast([_|Xs], Res) :- myLast(Xs, Res).


    count(_, [], 0).
    count(E, [E|Xs], Res) :- count(E, Xs, R1), Res is R1 + 1.
    count(E, [X|Xs], Res) :- E \= X, count(E, Xs, Res).


    concat(Lst1, [], Lst1).
    concat([], Lst2, Lst2).
    concat([X|Xs], [Y|Ys], [X|Res]) :- concat(Xs, [Y|Ys], Res).

    flatten([], []).
    flatten([X|Xs], Res) :- 
	    is_list(X),
	    flatten(X, R1),
	    flatten(Xs, R2),
	    concat(R1, R2, Res), !.

    flatten([X|Xs], [X|Res]) :- 
	    flatten(Xs, Res).

    reverse([], []).
    reverse([X|Xs], Res) :- reverse(Xs, R1), my_append(R1, X, Res).


    reverseFast(Lst, Res) :- reverseRec(Lst, [], Res).

    reverseRec([], Acc, Acc).
    reverseRec([X|Xs], Acc, Res) :- reverseRec(Xs, [X|Acc], Res).

    is_sorted([]) :- !.
    is_sorted([_]) :- !.
    is_sorted([X,Y|Xs]) :- X =< Y, is_sorted([Y|Xs]).

    factList(0, []).
    factList(N, Res) :-
	    N > 0,
	    NewN is N - 1,
	    fact1(N, R1),
	    factList(NewN, R2),
	    my_append(R2, R1, Res). 


    factListEF(0, [1]) :- !.
    factListEF(N, [1|Res]) :- factListEFREC(1, N, 1, Res).

    factListEFREC(N, N, Cur, [Cur]) :- !.
    factListEFREC(Cnt, N, Cur, [Cur|Res]) :-
	    NewCnt is Cnt + 1,
	    NewFact is NewCnt * Cur,
	    factListEFREC(NewCnt, N, NewFact, Res).

    zero(0).
    successor(Num, Res) :- Res is Num + 1.
    add(N1, N2, Res) :- Res is N1 + N2.
    mul(N1, N2, Res) :- Res is N1 * N2.

    my_zero([]).

    countOcurance([], [], _).
    countOcurance([X|Xs], [[X, R1]| Res], Counted) :-
	    not(contains(Counted, X)),
	    count(X, [X|Xs], R1),
	    countOcurance(Xs, Res, [X|Counted]), !.

    countOcurance([_|Xs], Res, Counted) :- countOcurance(Xs, Res, Counted).

    
my_counter([], []).
my_counter([H|T], [[H,Cnt] | Res]) :- 
	my_count(H, [H|T], Cnt),
	my_delete_all(T, H, Lst2),
	my_counter(Lst2, Res).

%-------------------------------- SORTY ------------------------------------


merge(Lst1, [], Lst1).
merge([], Lst2, Lst2).
merge([X|Xs], [Y|Ys], [X| R1]) :- X < Y, merge(Xs, [Y|Ys], R1), !.
merge([X|Xs], [Y|Ys], [Y| R1]) :- merge([X|Xs], Ys, R1).

mysplit(Lst, L1, L2) :-
	length1(Lst, Len),
	Len2 is Len //2,
	splitRec(Lst, 0, Len2, L1, L2).

splitRec(Lst, N, N, [], Lst) :- !.
splitRec([X|Xs], L, R, [X|L1], L2) :-
	NewL is L + 1,
	splitRec(Xs, NewL, R, L1, L2).

mergeSort([], []) :- !.
mergeSort([El], [El]) :- !.

mergeSort(Lst, Res) :-
	mysplit(Lst, L, R),
	mergeSort(L, R1),
	mergeSort(R, R2),
	merge(R1, R2, Res).


%------------------------------ BST -----------------------------------------

bstInsert([], E, bst(E, [], [])). 
bstInsert(bst(E, L, R), E, bst(E, L, R)) :- !.

bstInsert(bst(V, L, R), E, bst(V, Res, R)) :- E < V, bstInsert(L, E, Res).
bstInsert(bst(V, L, R), E, bst(V, L, Res)) :- E > V, bstInsert(R, E, Res).

bstFind(bst(V, _, _), V) :- !.
bstFind(bst(V, L, _), E) :- E < V, bstFind(L, E).
bstFind(bst(V, _, R), E) :- E > V, bstFind(R, E).

bstInorder([], []).
bstInorder(bst(V, L, R), Res) :- 
	bstInorder(L, R1),
	bstInorder(R, R2),
	concat(R1, [V|R2], Res).

bstBuild([], []) :- !.
bstBuild([X|Xs], Res) :-
	bstBuild(Xs, Bst),
	bstInsert(Bst, X, Res).

bstSort(Lst, Res) :- 
	bstBuild(Lst, Bst),
	bstInorder(Bst, Res).


myappend([], E, [E]).
myappend([X|Xs], E, [X |Res]) :- myappend(Xs, E, Res).

myreverse2([], []) :- !.
myreverse2([X|Xs], Res) :-
	myreverse2(Xs, R1),
	myappend(R1, X, Res).
%------------------------------------------------------------------------
prefix([], _).
prefix([X|Xs], [X|Ys]) :- prefix(Xs, Ys).

suffix(E, Lst) :- myreverse2(E, R1), myreverse2(Lst, R2), prefix(R1, R2).

block(E, Lst) :- prefix(E, Lst), suffix(E, Lst).

%------------------------------------------------------------------------


mymember([E|_], E).
mymember([_|Xs], E) :- mymember(Xs, E).

%[2 b.] Vytvořte predikát seq(+Lo, +Hi, -Lst), který do Lst vygeneruje jako seznam posloupnost čísel od Lo do Hi s inkrementem 1 (včetně obou konců).

seq(N, N, [N]) :- !.
seq(Lo, Hi, [Lo| Lst]) :-
	NewLo is Lo + 1,
	seq(NewLo, Hi, Lst).

sortedInsert([], E, [E]) :- !.
sortedInsert([X|Xs], E, [E,X|Xs]) :- E < X.
sortedInsert([X|Xs], E, [X|Res]) :- E >= X, sortedInsert(Xs, E, Res).

insertSort([], []).
insertSort([X|Xs], Res) :-
	insertSort(Xs, R1),
	sortedInsert(R1, X, Res).

printFirstColumn([], []).
printFirstColumn([[H|_]|T2],[H|Res1]):- 
	printFirstColumn(T2,Res1).

deleteFirstColumn([], []).
deleteFirstColumn([[_|T1] | T2], [T1|Res1]) :-
	deleteFirstColumn(T2, Res1).


leng([], 0).
leng([_|Xs], Res) :- leng(Xs, Res1), Res is 1 + Res1.

transpoze([H|_] , []) :- leng(H, 0), !.
transpoze(Mat, [T|Res]) :- 
	printFirstColumn(Mat, T),
	deleteFirstColumn(Mat, Next),
        transpoze(Next, Res).


splitex(Lst, A, B) :-
	leng(Lst, Len),
	Half is Len // 2,
	splitexRec(Lst, 0, Half, A, B).

splitexRec(X, L, L, [], X) :- !.
splitexRec([X|Xs], L, R, [X|A], B) :-
	NewL is L + 1,
	splitexRec(Xs, NewL, R, A, B).


lamerge(Lst1, [], Lst1) :- !.
lamerge([], Lst2, Lst2) :- !.

lamerge([X|Xs], [Y|Ys], [X |Res]) :- 
	X < Y,
	lamerge(Xs, [Y|Ys], Res).

lamerge([X|Xs], [Y|Ys], [Y | Res]) :- 
	X >= Y,
	lamerge([X|Xs], Ys, Res).

lamergesort([], []) :- !.
lamergesort([X], [X]) :- !.

lamergesort(Lst, Res) :- 
	splitex(Lst, Left, Right),
	lamergesort(Left, RLeft),
	lamergesort(Right, RRight),
	lamerge(RLeft, RRight, Res).
	
lamember([X|_], X).
lamember([_|Xs], E) :- 
	lamember(Xs, E).

checkSeq([_], _) :- !.
checkSeq([A, B|Xs], Diff) :- 
	Diff is B - A,
	checkSeq([B|Xs], Diff).

fillSeq([_], _) :- !.
fillSeq([A,B|Xs], Diff) :-
	B is A + Diff,
	fillSeq([B|Xs], Diff).

laAppend([], X, [X]) :- !.
laAppend([X|Xs], E, [X|Res]) :-
	laAppend(Xs, E, Res).

laAppendeListo([], Lst2, Lst2) :- !.
laAppendeListo(Lst1, [], Lst1) :- !.
laAppendeListo([X|Xs], Lst2, [X|Res]) :-
	laAppendeListo(Xs, Lst2, Res).

prefixos(Lst, Pref) :- laAppendeListo(Pref, _, Lst).
sufixos(Lst, Suff) :- laAppendeListo(_, Suff, Lst).




nicereverse([], Acc, Acc).
nicereverse([X|Xs], Acc, Res) :- 
	nicereverse(Xs, [X|Acc], Res).

lareverse(Lst, Res) :- nicereverse(Lst, [], Res).

