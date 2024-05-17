mirror(empty, empty).
mirror(btree(V, La, Ra), btree(V, Lb, Rb)) :- 
	mirror(La, Rb),
	mirror(Ra, Lb).
