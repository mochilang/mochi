:- style_check(-singleton).
triple(X, _Res) :-
    _Res is (X * 3).

:- initialization(main, main).
main :-
    triple((1 + 2), _V0),
    writeln(_V0),
    true.
