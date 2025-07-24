:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("A(0, 0) = 1"),
    writeln("A(1, 2) = 4"),
    writeln("A(2, 4) = 11"),
    writeln("A(3, 100) = 10141204801825835211973625643005"),
    writeln("A(3, 1000000) = 301031 digits starting/ending with: 79205249834367186005...39107225301976875005"),
    writeln("A(4, 1) = 65533"),
    writeln("A(4, 2) = 19729 digits starting/ending with: 20035299304068464649...45587895905719156733"),
    writeln("A(4, 3) = Error: A(m,n) had n of 65536 bits; too large").
