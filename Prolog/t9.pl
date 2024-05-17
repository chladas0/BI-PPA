digtolet(2, [a,b,c]).
digtolet(3, [d,e,f]). 
digtolet(4, [g,h,i]). 
digtolet(5, [j, k, l]). 
digtolet(6, [m, n, o]). 
digtolet(7, [p, q, r,s]). 
digtolet(8, [t, u, v]).
digtolet(9, [w, x, y, z]).

myappend(E, [], [E]).
myappend(E, [X|Xs], [X|Rs]) :- myappend(E, Xs, Rs). 

t9rec(0, []).

t9rec(NUM, RES) :- 
	NUM > 0,
	DIGIT is mod(NUM, 10),
	NEXT is div(NUM, 10),
	digtolet(DIGIT, POSSIBLE),
	member(L, POSSIBLE),
	t9rec(NEXT, REST),
	myappend(L, REST, RES).

t9(NUM, RES) :- NUM > 1, t9rec(NUM, RES).
