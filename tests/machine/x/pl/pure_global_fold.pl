:- style_check(-singleton).
Inc(X, _Res) :-
    _Res is (X + K).

:- initialization(main, main).
main :-
    K is 2,
    Inc(3, _V0),
    writeln(_V0),
    true.
