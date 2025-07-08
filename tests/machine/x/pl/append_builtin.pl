:- initialization(main, main).
main :-
    A = [1, 2],
    append(A, [3], _V0),
    writeln(_V0),
    true.
