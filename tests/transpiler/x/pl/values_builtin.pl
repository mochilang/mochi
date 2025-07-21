:- style_check(-singleton).
:- initialization(main).

main :-
    M = {a: 1, b: 2, c: 3},
    values(M, R1), writeln(R1).
