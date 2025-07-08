:- style_check(-singleton).
Add(A, B, _Res) :-
    _Res is (A + B).

:- initialization(main, main).
main :-
    Add(2, 3, _V0),
    writeln(_V0),
    true.
