:- style_check(-singleton).
Boom(A, B, _Res) :-
    write("boom"),
    nl,
    _Res = true.

:- initialization(main, main).
main :-
    Boom(1, 2, _V0),
    write((false, _V0)),
    nl,
    Boom(1, 2, _V1),
    write((true ; _V1)),
    nl,
    true.
