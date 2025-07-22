:- initialization(main).
:- style_check(-singleton).

outer(X, R) :-
    inner(5, Return1),
    R = Return1.

main :-
    outer(3, R0), writeln(R0).
