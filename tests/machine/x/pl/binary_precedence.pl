:- style_check(-singleton).
main :-
    write(((1 + 2) * 3)),
    nl,
    write(((1 + 2) * 3)),
    nl,
    write(((2 * 3) + 1)),
    nl,
    write((2 * (3 + 1))),
    nl,
    true.
:- initialization(main, main).
