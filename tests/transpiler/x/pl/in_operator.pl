:- style_check(-singleton).
:- initialization(main).

main :-
    Xs = [1, 2, 3],
    (member(2, Xs) -> writeln(true) ; writeln(false)),
    writeln(\+((member(5, Xs)))).
