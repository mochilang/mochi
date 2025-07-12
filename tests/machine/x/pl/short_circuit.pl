:- style_check(-singleton).
boom(A, B, _Res) :-
    writeln("boom"),
    _Res = true.

:- initialization(main, main).
main :-
    boom(1, 2, _V0),
    writeln((false, _V0)),
    boom(1, 2, _V1),
    writeln((true ; _V1)),
    true.
