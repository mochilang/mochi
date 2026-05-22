:- initialization(main).
:- style_check(-singleton).

main :-
    downcase_atom('HELLO', L),
    writeln(L).
