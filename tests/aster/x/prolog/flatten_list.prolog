:- initialization(main).
:- style_check(-singleton).
main :-
        flatten([1, [2, [3]], 4], F),
        writeln(F).
