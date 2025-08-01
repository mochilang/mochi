:- style_check(-singleton).
:- initialization(main).
sum3(A, B, C, R) :-
        Return1 is A + B + C,
        R = Return1.
main :-
        sum3(1, 2, 3, R0),
        writeln(R0).
