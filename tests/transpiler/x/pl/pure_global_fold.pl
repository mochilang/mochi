:- style_check(-singleton).
:- initialization(main).

inc(X, R) :-
    Return1 is X + K,
    R = Return1.

main :-
    K is 2,
    inc(3, R1), writeln(R1).
