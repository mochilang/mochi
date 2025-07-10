:- style_check(-singleton).
sum_tree(T, _Res) :-
    sum_tree(Left, _V0),
    sum_tree(Right, _V1),
    (T == leaf -> _V2 = 0 ; _V2 = ((_V0 + Value) + _V1)),
    _Res is _V2.

:- initialization(main, main).
main :-
    dict_create(_V0, p_node, ['left'-leaf, 'value'-2, 'right'-leaf]),
    dict_create(_V1, p_node, ['left'-leaf, 'value'-1, 'right'-_V0]),
    T = _V1,
    sum_tree(T, _V2),
    write(_V2),
    nl,
    true.
