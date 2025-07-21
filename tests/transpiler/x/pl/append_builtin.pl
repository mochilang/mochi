:- style_check(-singleton).
:- initialization(main).

main :-
    A = [1, 2],
    append(A, [3], R1), writeln(R1).
