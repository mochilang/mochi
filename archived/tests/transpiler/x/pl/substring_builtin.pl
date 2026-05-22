:- initialization(main).
:- style_check(-singleton).

main :-
    L0 is 4 - 1, sub_string("mochi", 1, L0, _, R0), writeln(R0).
