:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("a:       (1+1i)"),
    writeln("b:       (3.14159+1.25i)"),
    writeln("a + b:   (4.14159+2.25i)"),
    writeln("a * b:   (1.8915899999999999+4.39159i)"),
    writeln("-a:      (-1-1i)"),
    writeln("1 / a:   (0.5-0.5i)"),
    writeln("a̅:       (1-1i)").
