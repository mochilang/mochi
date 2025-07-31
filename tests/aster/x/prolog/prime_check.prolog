:- initialization(main).
:- style_check(-singleton).
has_factor(N, F) :-
        =<(F * F, N),
    (N mod F =:= 0; F1 is F + 1, has_factor(N, F1)).
is_prime(2).
is_prime(N) :-
        N > 2,
        \+(has_factor(N, 2)).
main :-
    is_prime(7) -> writeln(true); writeln(false).
