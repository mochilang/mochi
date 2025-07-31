:- initialization(main).
:- style_check(-singleton).
main :-
        A is 7 / 2,
            B is 7 mod 2,
            writeln(A),
        writeln(B).
