:- style_check(-singleton).
:- initialization(main).

main :-
    writeln(2),
    writeln(3),
    writeln(1),
    writeln(2),
    L2 is 4 - 1, sub_string("hello", 1, L2, _, R2), writeln(R2).
