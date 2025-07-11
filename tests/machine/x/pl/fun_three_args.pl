:- style_check(-singleton).
sum3(A, B, C, _Res) :-
    _Res is ((A + B) + C).

:- initialization(main, main).
main :-
    sum3(1, 2, 3, _V0),
    write(_V0),
    nl,
    true.
