:- initialization(main).
:- style_check(-singleton).
main :-
        atom_chars(abc, Chars),
            reverse(Chars, RevChars),
            atom_chars(R, RevChars),
        writeln(R).
