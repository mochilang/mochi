:- style_check(-singleton).
:- initialization(main).
add(A, B, R) :-
        Return1 is A + B,
        R = Return1.
main :-
        add(2, 3, R0),
        writeln(R0).
