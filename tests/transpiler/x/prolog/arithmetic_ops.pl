:- initialization(main).
:- style_check(-singleton).

main :-
    A is 1 + 2 * 3,
    writeln(A).
