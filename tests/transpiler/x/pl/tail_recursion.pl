:- initialization(main).

sum_rec(N, Acc, R) :-
    (N =:= 0 ->
    Return1 = Acc),
    sum_rec(N - 1, Acc + N, Return2),
    R = Return2.

main :-
    sum_rec(10, 0, R0), writeln(R0).
