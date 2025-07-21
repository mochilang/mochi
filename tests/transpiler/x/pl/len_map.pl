:- style_check(-singleton).
:- initialization(main).

main :-
    length({a: 1, b: 2}, R0), writeln(R0).
