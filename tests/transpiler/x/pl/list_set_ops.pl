:- initialization(main).
:- style_check(-singleton).

main :-
    union([1, 2], [2, 3], U1), writeln(U1),
    subtract([1, 2, 3], [2], U2), writeln(U2),
    intersection([1, 2, 3], [2, 4], U3), writeln(U3),
    append([1, 2], [2, 3], U4), length(U4, L), writeln(L).
