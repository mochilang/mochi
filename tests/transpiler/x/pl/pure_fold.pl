:- style_check(-singleton).
:- initialization(main).

triple(X, R) :-
    Return1 is X * 3,
    R = Return1.

main :-
    triple(3, R0), writeln(R0).
