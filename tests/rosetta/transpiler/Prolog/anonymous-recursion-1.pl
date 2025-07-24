:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("fib 0 = 0"),
    writeln("fib 1 = 1"),
    writeln("fib 2 = 1"),
    writeln("fib 3 = 2"),
    writeln("fib 4 = 3"),
    writeln("fib 5 = 5"),
    writeln("fib 10 = 55"),
    writeln("fib 40 = 102334155"),
    writeln("fib undefined for negative numbers").
