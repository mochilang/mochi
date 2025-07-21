:- style_check(-singleton).
:- initialization(main).

main :-
    M = {1: "a", 2: "b"},
    nth0(1, M, R1), writeln(R1).
