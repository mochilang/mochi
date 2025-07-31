:- initialization(main).
:- style_check(-singleton).
main :-
        atom_chars('hi', Cs),
        writeln(Cs).
