:- style_check(-singleton).
outer__inner(X, Y, _Res) :-
    _Res is (X + Y).

outer(X, _Res) :-
    outer__inner(X, 5, _V0),
    _Res = _V0.

:- initialization(main, main).
main :-
    outer(3, _V0),
    writeln(_V0),
    true.
