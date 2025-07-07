:- style_check(-singleton).
main :-
    A = (10 - 3),
    B = (2 + 2),
    write(A),
    nl,
    write((A =:= 7)),
    nl,
    write((B < 5)),
    nl,
    true.
:- initialization(main, main).
