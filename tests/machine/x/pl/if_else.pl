:- style_check(-singleton).
:- initialization(main, main).
main :-
    X is 5,
    ((X @> 3) ->
        writeln("big"),
        true
    ;
        writeln("small"),
        true
    ),
    true.
