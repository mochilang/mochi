:- style_check(-singleton).
:- initialization(main).

main :-
    Prefix = "fore",
    S1 = "forest",
    L2 is 4 - 0, sub_string(S1, 0, L2, _, T2), ((T2 = Prefix) -> writeln(true) ; writeln(false)),
    S2 = "desert",
    L4 is 4 - 0, sub_string(S2, 0, L4, _, T4), ((T4 = Prefix) -> writeln(true) ; writeln(false)).
