:- style_check(-singleton).
:- initialization(main).

main :-
    Scores = {alice: 1},
    nth0("bob", Scores, _, T1),
    nth0("bob", Scores1, 2, T1),
    nth0("bob", Scores1, R2), writeln(R2).
