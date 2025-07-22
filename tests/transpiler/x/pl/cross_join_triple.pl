:- initialization(main).
:- style_check(-singleton).

main :-
    Nums = [1, 2],
    Letters = ["A", "B"],
    Bools = [true, false],
    Combos = [_{n: 1, l: "A", b: true}, _{n: 1, l: "A", b: false}, _{n: 1, l: "B", b: true}, _{n: 1, l: "B", b: false}, _{n: 2, l: "A", b: true}, _{n: 2, l: "A", b: false}, _{n: 2, l: "B", b: true}, _{n: 2, l: "B", b: false}],
    writeln("--- Cross Join of three lists ---"),
    C = _{n: 1, l: "A", b: true},
    writeln("1 A true"),
    C1 = _{n: 1, l: "A", b: false},
    writeln("1 A false"),
    C2 = _{n: 1, l: "B", b: true},
    writeln("1 B true"),
    C3 = _{n: 1, l: "B", b: false},
    writeln("1 B false"),
    C4 = _{n: 2, l: "A", b: true},
    writeln("2 A true"),
    C5 = _{n: 2, l: "A", b: false},
    writeln("2 A false"),
    C6 = _{n: 2, l: "B", b: true},
    writeln("2 B true"),
    C7 = _{n: 2, l: "B", b: false},
    writeln("2 B false").
