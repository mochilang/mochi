:- initialization(main).
:- style_check(-singleton).
factorial(0, A, A).
factorial(N, A, F) :-
        N > 0,
            A1 is A * N,
            N1 is N - 1,
        factorial(N1, A1, F).
main :-
        factorial(5, 1, F),
        writeln(F).
