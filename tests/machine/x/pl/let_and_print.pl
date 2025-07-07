:- style_check(-singleton).
main :-
    A = 10,
    B = 20,
    write((A + B)),
    nl,
    true.
:- initialization(main, main).
