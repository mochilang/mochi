:- style_check(-singleton).
main :-
    X = 5,
    ((X > 3) ->
        write("big"),
        nl,
    ;
        write("small"),
        nl,
    ),
    true.
:- initialization(main, main).
