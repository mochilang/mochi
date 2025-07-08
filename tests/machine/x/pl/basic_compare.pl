:- style_check(-singleton).
main :-
    A is (10 - 3),
    B is (2 + 2),
    write(A),
    nl,
    write((A =:= 7)),
    nl,
    write((B < 5)),
    nl,
    true.
:- initialization(main, main).
