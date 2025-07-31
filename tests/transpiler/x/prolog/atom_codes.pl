:- initialization(main).
:- style_check(-singleton).

main :-
    atom_codes('abc', Codes),
    writeln(Codes).
