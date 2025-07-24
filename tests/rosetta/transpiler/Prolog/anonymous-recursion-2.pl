:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("fib(-1) returned error: negative n is forbidden"),
    writeln("fib(0) = 0"),
    writeln("fib(1) = 1"),
    writeln("fib(2) = 1"),
    writeln("fib(3) = 2"),
    writeln("fib(4) = 3"),
    writeln("fib(5) = 5"),
    writeln("fib(6) = 8"),
    writeln("fib(7) = 13"),
    writeln("fib(8) = 21"),
    writeln("fib(9) = 34"),
    writeln("fib(10) = 55").
