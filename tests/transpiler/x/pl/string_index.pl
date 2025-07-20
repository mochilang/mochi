:- style_check(-singleton).
:- initialization(main).

main :-
    S = 'mochi',
    sub_string(S, 1, 1, _, R1), writeln(R1).
