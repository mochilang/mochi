:- style_check(-singleton).
:- initialization(main).

main :-
    M = {a: 1, b: 2},
    (get_dict("a", M, _) -> writeln(true) ; writeln(false)),
    (get_dict("c", M, _) -> writeln(true) ; writeln(false)).
