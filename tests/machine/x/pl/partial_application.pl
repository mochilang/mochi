:- style_check(-singleton).
Add(A, B, _Res) :-
    _Res is (A + B).

:- initialization(main, main).
main :-
    Add(5, _V0),
    Add5 = _V0,
    Add5(3, _V1),
    write(_V1),
    nl,
    true.
