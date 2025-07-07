:- initialization(main).

tree(leaf).
tree(node(Left,Value,Right)) :- tree(Left), tree(Right).

sum_tree(leaf,0).
sum_tree(node(L,V,R),Sum) :-
    sum_tree(L,SL), sum_tree(R,SR), Sum is SL+V+SR.

sample_tree(node(leaf,1,node(leaf,2,leaf))).

main :-
    sample_tree(T), sum_tree(T,S), writeln(S), halt.
