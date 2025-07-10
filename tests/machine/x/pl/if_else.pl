:- style_check(-singleton).
:- initialization(main, main).
main :-
    X is 5,
    ((X > 3) ->
        write("big"),
        nl,
    ;
        write("small"),
        nl,
    ),
    true.
