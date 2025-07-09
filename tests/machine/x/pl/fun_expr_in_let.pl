:- style_check(-singleton).
P__lambda0(X, _Res) :-
    _Res is (X * X).

:- initialization(main, main).
main :-
    Square = p__lambda0,
    Square(6, _V1),
    writeln(_V1),
    true.
