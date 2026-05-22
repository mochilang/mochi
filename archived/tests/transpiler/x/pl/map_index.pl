:- style_check(-singleton).
:- initialization(main).

main :-
    M = {a: 1, b: 2},
    nth0("b", M, R1), writeln(R1).
