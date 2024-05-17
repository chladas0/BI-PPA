split(Lst, L, S, R) :-
    append(L, RightPart, Lst),
    append(S, R, RightPart).

repeat3(Lst, X) :- 
    split(Lst, _, X, R1),
    split(R1,  _, X, R2),
    split(R2,  _, X, _),
    X \= [].
