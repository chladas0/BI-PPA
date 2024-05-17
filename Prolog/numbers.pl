operation(+, X, Y, Result) :- Result is X + Y.
operation(-, X, Y, Result) :- Result is X - Y.
operation(*, X, Y, Result) :- Result is X * Y.
operation(/, X, Y, Result) :- Y \= 0, 0 is X mod Y, Result is X / Y.

numbers(Lst, Target) :- member(Target, Lst).
numbers(Lst, Target) :-
    select(X, Lst, Rest),
    select(Y, Rest, NewRest),
    member(Op, [+, - , *, /]),
    operation(Op, X, Y, NewResult),
    numbers([NewResult|NewRest], Target).
