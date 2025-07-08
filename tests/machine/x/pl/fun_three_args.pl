:- style_check(-singleton).
Sum3(A, B, C, _Res) :-
    _Res is ((A + B) + C).

:- initialization(main, main).
main :-
    Sum3(1, 2, 3, _V0),
    writeln(_V0),
    true.
