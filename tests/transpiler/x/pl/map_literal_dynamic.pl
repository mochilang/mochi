:- initialization(main).
:- style_check(-singleton).

main :-
    X is 3,
    Y is 4,
    M = map{a: 3, b: 4},
    writeln("3 4").
