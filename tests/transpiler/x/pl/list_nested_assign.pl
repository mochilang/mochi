:- initialization(main).
:- style_check(-singleton).

main :-
    M0 = [[1, 2], [3, 4]],
    nth0(1, M0, R0),
    nth0(0, R0, _, T1),
    nth0(0, R1, 5, T1),
    nth0(1, M0, _, Rest),
    nth0(1, M1, R1, Rest),
    nth0(1, M1, Row),
    nth0(0, Row, V),
    writeln(V).
