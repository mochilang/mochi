:- style_check(-singleton).
p__partial0(P0, _Res) :-
    add(5, P0, _Res).

add(A, B, _Res) :-
    _Res is (A + B).

:- initialization(main, main).
main :-
    Add5 = p__partial0,
    call(Add5, 3, _V1),
    writeln(_V1),
    true.
