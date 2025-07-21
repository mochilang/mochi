:- style_check(-singleton).
:- initialization(main).

main :-
    M = {1: "a", 2: "b"},
    (get_dict(1, M, _) -> writeln(true) ; writeln(false)),
    (get_dict(3, M, _) -> writeln(true) ; writeln(false)).
