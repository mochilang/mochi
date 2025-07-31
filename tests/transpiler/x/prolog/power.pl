:- initialization(main).
:- style_check(-singleton).

power(_, 0, 1).
power(X, N, R) :-
    N > 0,
    N1 is N - 1,
    power(X, N1, R1),
    R is X * R1.

main :-
    power(2, 5, R),
    writeln(R).
