:- style_check(-singleton).
p__lambda0(X, _Res) :-
    _Res is (X + N).

makeAdder(N, _Res) :-
    _Res = p__lambda0.

:- initialization(main, main).
main :-
    makeAdder(10, _V0),
    Add10 = _V0,
    add10(7, _V1),
    write(_V1),
    nl,
    true.
