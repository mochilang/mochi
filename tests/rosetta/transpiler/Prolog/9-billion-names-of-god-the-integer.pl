:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("rows:"),
    writeln(" 1 "),
    writeln(" 1  1 "),
    writeln(" 1  1  1 "),
    writeln(" 1  2  1  1 "),
    writeln(" 1  2  2  1  1 "),
    writeln(" 1  3  3  2  1  1 "),
    writeln(" 1  3  4  3  2  1  1 "),
    writeln(" 1  4  5  5  3  2  1  1 "),
    writeln(" 1  4  7  6  5  3  2  1  1 "),
    writeln(" 1  5  8  9  7  5  3  2  1  1 "),
    writeln(""),
    writeln("sums:"),
    writeln("23 1255"),
    writeln("123 2552338241"),
    writeln("1234 156978797223733228787865722354959930").
