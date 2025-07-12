:- style_check(-singleton).
p__lambda0_impl(N, X, _Res) :-
    _Res is (X + N).

p__lambda1(P0, _Res) :-
    p__lambda0_impl(N, P0, _Res).

makeAdder(N, _Res) :-
    _Res = p__lambda1.

:- initialization(main, main).
main :-
    makeAdder(10, _V0),
    Add10 = _V0,
    call(Add10, 7, _V1),
    writeln(_V1),
    true.
