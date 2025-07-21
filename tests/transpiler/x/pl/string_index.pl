:- initialization(main).
:- style_check(-singleton).

main :-
    S = "mochi",
    sub_string(S, 1, 1, _, R1), writeln(R1).
