:- initialization(main).

main :-
    ((1 < 2) , (2 < 3) , (3 < 4) -> writeln(true) ; writeln(false)),
    ((1 < 2) , (2 > 3) , true -> writeln(true) ; writeln(false)),
    ((1 < 2) , (2 < 3) , (3 > 4) , true -> writeln(true) ; writeln(false)).
