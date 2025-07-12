:- style_check(-singleton).
add(A, B, _Res) :-
    _Res is (A + B).

:- initialization(main, main).
main :-
    add(2, 3, _V0),
    writeln(_V0),
    true.
