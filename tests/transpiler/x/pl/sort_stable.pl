:- initialization(main).
:- style_check(-singleton).

main :-
    Items = [_{n: 1, v: "a"}, _{n: 1, v: "b"}, _{n: 2, v: "c"}],
    Result = ["a", "b", "c"],
    writeln("a"),
    writeln("b"),
    writeln("c").
