:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("pair	dist	path"),
    writeln("1 -> 2	-1	1 -> 3 -> 4 -> 2"),
    writeln("1 -> 3	-2	1 -> 3"),
    writeln("1 -> 4	0	1 -> 3 -> 4"),
    writeln("2 -> 1	4	2 -> 1"),
    writeln("2 -> 3	2	2 -> 1 -> 3"),
    writeln("2 -> 4	4	2 -> 1 -> 3 -> 4"),
    writeln("3 -> 1	5	3 -> 4 -> 2 -> 1"),
    writeln("3 -> 2	1	3 -> 4 -> 2"),
    writeln("3 -> 4	2	3 -> 4"),
    writeln("4 -> 1	3	4 -> 2 -> 1"),
    writeln("4 -> 2	-1	4 -> 2"),
    writeln("4 -> 3	1	4 -> 2 -> 1 -> 3").
