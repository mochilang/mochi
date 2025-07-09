:- style_check(-singleton).
P__lambda0(X, _Res) :-
    _Res is (X + N).

MakeAdder(N, _Res) :-
    _Res = p__lambda0.

:- initialization(main, main).
main :-
    MakeAdder(10, _V0),
    Add10 = _V0,
    Add10(7, _V1),
    writeln(_V1),
    true.
