:- initialization(main).
:- style_check(-singleton).

makeAdder(N, R) :-
    Return1 = <fun>,
    R = Return1.

main :-
    Add10 = <fun>,
    add10(7, R1), writeln(R1).
