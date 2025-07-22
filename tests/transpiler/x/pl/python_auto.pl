:- initialization(main).
:- style_check(-singleton).

main :-
    Math.sqrt(16, R0), writeln(R0),
    writeln(3.141592653589793).
