:- style_check(-singleton).
Triple(X, _Res) :-
    _Res is (X * 3).

:- initialization(main, main).
main :-
    Triple((1 + 2), _V0),
    write(_V0),
    nl,
    true.
