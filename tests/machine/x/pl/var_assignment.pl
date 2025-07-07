:- style_check(-singleton).
main :-
    nb_setval(x, 1),
    nb_setval(x, 2),
    write(X),
    nl,
    true.
:- initialization(main, main).
