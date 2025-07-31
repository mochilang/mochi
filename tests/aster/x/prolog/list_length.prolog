:- initialization(main).
:- style_check(-singleton).
main :-
        length([1, 2, 3], L),
        writeln(L).
