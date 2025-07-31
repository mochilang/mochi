:- initialization(main).
:- style_check(-singleton).

fib(0, A, _, A).
fib(N, A, B, F) :-
    N > 0,
    N1 is N - 1,
    C is A + B,
    fib(N1, B, C, F).

main :-
    fib(6, 0, 1, F),
    writeln(F).
