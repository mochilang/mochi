:- initialization(main).
:- style_check(-singleton).

sum_tree(T, R) :-
    (T =:= Leaf -> Return1 = 0 ; (T = _{tag: "Node", left: Left, value: Value, right: Right} -> Return1 = sum_tree(Left) + Value + sum_tree(Right) ; Return1 = 0)),
    R = Return1.

main :-
    T = _{tag: "Node", left: Leaf, value: 1, right: _{tag: "Node", left: Leaf, value: 2, right: Leaf}},
    sum_tree(_{tag: "Node", left: Leaf, value: 1, right: _{tag: "Node", left: Leaf, value: 2, right: Leaf}}, R1), writeln(R1).
