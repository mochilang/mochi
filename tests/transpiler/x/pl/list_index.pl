:- initialization(main).

main :-
    Xs = [10, 20, 30],
    nth0(1, Xs, R1), writeln(R1).
