:- style_check(-singleton).
triple(X, _Res) :-
    _Res is (X * 3).

:- initialization(main, main).
main :-
    triple((1 + 2), _V0),
    write(_V0),
    nl,
    true.
