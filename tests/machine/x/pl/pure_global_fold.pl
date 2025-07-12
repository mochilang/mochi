:- style_check(-singleton).
inc(X, _Res) :-
    _Res is (X + K).

:- initialization(main, main).
main :-
    K is 2,
    inc(3, _V0),
    writeln(_V0),
    true.
