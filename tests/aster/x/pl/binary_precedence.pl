:- initialization(main).
:- style_check(-singleton).
main :-
        writeln(9),
            R1 is 3 * 3,
            writeln(R1),
            writeln(7),
            R3 is 2 * 4,
        writeln(R3).
