:- initialization(main).
:- style_check(-singleton).

main :-
    D = _{a: 1, b: 2},
    get_dict(a, D, V),
    writeln(V).
