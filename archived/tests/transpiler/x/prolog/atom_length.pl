:- initialization(main).
:- style_check(-singleton).

main :-
    atom_length('mochi', L),
    writeln(L).
