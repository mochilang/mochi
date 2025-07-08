:- style_check(-singleton).
Boom(, _Res) :-
    writeln("boom"),
    _Res = true.

:- initialization(main, main).
main :-
    ((((1 < 2), (2 < 3)), (3 < 4)) -> _V0 = true ; _V0 = false),
    writeln(_V0),
    Boom(, _V1),
    ((((1 < 2), (2 > 3)), _V1) -> _V2 = true ; _V2 = false),
    writeln(_V2),
    Boom(, _V3),
    (((((1 < 2), (2 < 3)), (3 > 4)), _V3) -> _V4 = true ; _V4 = false),
    writeln(_V4),
    true.
