:- initialization(main).
:- style_check(-singleton).

main :-
    Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9],
    N is 1,
    (false ->
),
    (false ->
),
    writeln("odd number: 1"),
    N1 is 2,
    (true ->
),
    (false ->
),
    writeln("odd number: 2"),
    N2 is 3,
    (false ->
),
    (false ->
),
    writeln("odd number: 3"),
    N3 is 4,
    (true ->
),
    (false ->
),
    writeln("odd number: 4"),
    N4 is 5,
    (false ->
),
    (false ->
),
    writeln("odd number: 5"),
    N5 is 6,
    (true ->
),
    (false ->
),
    writeln("odd number: 6"),
    N6 is 7,
    (false ->
),
    (false ->
),
    writeln("odd number: 7"),
    N7 is 8,
    (true ->
),
    (true ->
),
    writeln("odd number: 8"),
    N8 is 9,
    (false ->
),
    (true ->
),
    writeln("odd number: 9").
