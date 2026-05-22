:- style_check(-singleton).
:- initialization(main).

add(A, B, R) :-
    Return1 is A + B,
    R = Return1.

main :-
    add(5, Add5),
    add5(3, R1), writeln(R1).
