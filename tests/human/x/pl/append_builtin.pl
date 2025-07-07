:- initialization(main, main).

main :-
    A = [1,2],
    append(A, [3], Result),
    writeln(Result).
