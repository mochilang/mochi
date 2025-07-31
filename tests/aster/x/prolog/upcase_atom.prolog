:- initialization(main).
:- style_check(-singleton).
main :-
        upcase_atom('hello', U),
        writeln(U).
