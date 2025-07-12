:- style_check(-singleton).
:- initialization(main, main).
main :-
    _V0 is 1,
    nb_setval(x, _V0),
    _V1 is 2,
    nb_setval(x, _V1),
    nb_getval(x, _V2),
    writeln(_V2),
    true.
