:- style_check(-singleton).
:- initialization(main, main).
main :-
    _V0 is ((1 + 2) * 3),
    writeln(_V0),
    _V1 is ((1 + 2) * 3),
    writeln(_V1),
    _V2 is ((2 * 3) + 1),
    writeln(_V2),
    _V3 is (2 * (3 + 1)),
    writeln(_V3),
    true.
