:- style_check(-singleton).
:- initialization(main).

main :-
    M = {a: 1, b: 2},
    K = "a",
    writeln("a"),
    K1 = "b",
    writeln("b").
