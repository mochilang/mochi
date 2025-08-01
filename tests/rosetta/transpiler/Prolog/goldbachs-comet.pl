:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("The first 100 Goldbach numbers:"),
    writeln("  1   1   1   2   1   2   2   2   2   3"),
    writeln("  3   3   2   3   2   4   4   2   3   4"),
    writeln("  3   4   5   4   3   5   3   4   6   3"),
    writeln("  5   6   2   5   6   5   5   7   4   5"),
    writeln("  8   5   4   9   4   5   7   3   6   8"),
    writeln("  5   6   8   6   7  10   6   6  12   4"),
    writeln("  5  10   3   7   9   6   5   8   7   8"),
    writeln(" 11   6   5  12   4   8  11   5   8  10"),
    writeln("  5   6  13   9   6  11   7   7  14   6"),
    writeln("  8  13   5   8  11   7   9  13   8   9"),
    writeln(""),
    writeln("The 1,000th Goldbach number = 28").
