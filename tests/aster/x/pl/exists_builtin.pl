:- style_check(-singleton).
:- initialization(main).
main :-
        Data = [1, 2],
            exists([], Flag),
        writeln(Flag).
