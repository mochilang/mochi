:- initialization(main).
:- style_check(-singleton).

main :-
    A is 10,
    B is 20,
    R2 is A + B, writeln(R2).
