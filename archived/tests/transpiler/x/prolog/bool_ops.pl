:- initialization(main).
:- style_check(-singleton).

main :-
    (1 =:= 1, 2 > 1 -> writeln(true) ; writeln(false)).
