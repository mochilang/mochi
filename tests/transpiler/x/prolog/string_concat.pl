:- initialization(main).
:- style_check(-singleton).

main :-
    atom_concat('hello', 'world', A),
    writeln(A).
