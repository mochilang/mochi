:- style_check(-singleton).
:- initialization(main).

boom(A, B, R) :-
    writeln("boom"),
    Return1 = true,
    R = Return1.

main :-
    (false , boom(1, 2) -> writeln(true) ; writeln(false)),
    (true ; boom(1, 2) -> writeln(true) ; writeln(false)).
