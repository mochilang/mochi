:- style_check(-singleton).
Boom(A, B, _Res) :-
    writeln("boom"),
    _Res = true.

:- initialization(main, main).
main :-
    Boom(1, 2, _V0),
    writeln((false, _V0)),
    Boom(1, 2, _V1),
    writeln((true ; _V1)),
    true.
