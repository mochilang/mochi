:- initialization(main).
:- style_check(-singleton).

main :-
    Testpkg.Add(2, 3, R0), writeln(R0),
    writeln(3.14),
    writeln(42).
