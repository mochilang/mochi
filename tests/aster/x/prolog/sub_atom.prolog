:- initialization(main).
:- style_check(-singleton).
main :-
        sub_atom('hello', 1, 3, _, Sub),
        writeln(Sub).
