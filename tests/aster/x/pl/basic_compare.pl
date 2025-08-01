:- initialization(main).
:- style_check(-singleton).
main :-
        A is 7,
            B is 4,
            writeln(7),
        (A =:= 7 -> writeln(true); writeln(false)),
    (B < 5 -> writeln(true); writeln(false)).
