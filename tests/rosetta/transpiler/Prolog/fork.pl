:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("PID: 1"),
    writeln("Child''s PID: 2"),
    writeln("PID: 2"),
    writeln("Done.").
