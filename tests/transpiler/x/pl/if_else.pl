:- style_check(-singleton).
:- initialization(main).

main :-
    X is 5,
    (X > 3 ->
    writeln("big") ;
    writeln("small")).
