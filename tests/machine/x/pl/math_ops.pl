:- style_check(-singleton).
main :-
    write((6 * 7)),
    nl,
    write((7 / 2)),
    nl,
    write((7 mod 2)),
    nl,
    true.
:- initialization(main, main).
