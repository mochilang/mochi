:- style_check(-singleton).
boom(, _Res) :-
    write("boom"),
    nl,
    _Res = true.

:- initialization(main, main).
main :-
    ((((1 @< 2), (2 @< 3)), (3 @< 4)) -> _V0 = true ; _V0 = false),
    write(_V0),
    nl,
    boom(, _V1),
    ((((1 @< 2), (2 @> 3)), _V1) -> _V2 = true ; _V2 = false),
    write(_V2),
    nl,
    boom(, _V3),
    (((((1 @< 2), (2 @< 3)), (3 @> 4)), _V3) -> _V4 = true ; _V4 = false),
    write(_V4),
    nl,
    true.
