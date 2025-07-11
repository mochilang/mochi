:- style_check(-singleton).
:- initialization(main, main).
main :-
    nb_setval(x, _),
    nb_getval(x, _V0),
    write(_V0),
    nl,
    true.
