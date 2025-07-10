:- style_check(-singleton).
:- initialization(main, main).
main :-
    _V0 is ((1 + 2) * 3),
    write(_V0),
    nl,
    _V1 is ((1 + 2) * 3),
    write(_V1),
    nl,
    _V2 is ((2 * 3) + 1),
    write(_V2),
    nl,
    _V3 is (2 * (3 + 1)),
    write(_V3),
    nl,
    true.
