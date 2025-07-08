:- initialization(main, main).
main :-
    (member(N, [1, 2, 3]),
        writeln(N),
        fail
    ; true),
    true.
