:- initialization(main).
:- style_check(-singleton).

main :-
    atom_length('abcde', L),
    writeln(L).
