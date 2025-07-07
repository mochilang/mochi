:- style_check(-singleton).
    main :-
    nb_setval(x, 1),
    nb_setval(x, 2),
    nb_getval(x, _V0),
    write(_V0),
    nl
    .
:- initialization(main, main).
