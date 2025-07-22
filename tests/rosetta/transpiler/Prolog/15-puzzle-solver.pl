:- initialization(main).
:- style_check(-singleton).

main :-
    Testpkg.FifteenPuzzleExample(, R0), writeln(R0).
