:- initialization(main).
:- style_check(-singleton).

main :-
    writeln(-3),
    R1 is 5 + (-2), writeln(R1).
