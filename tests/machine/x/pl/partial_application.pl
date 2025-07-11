:- style_check(-singleton).
add(A, B, _Res) :-
    _Res is (A + B).

:- initialization(main, main).
main :-
    add(5, _V0),
    Add5 = _V0,
    add5(3, _V1),
    write(_V1),
    nl,
    true.
