:- style_check(-singleton).
main :-
    nb_setval(i, 0),
    repeat,
    ((I < 3) ->
        write(I),
        nl,
        nb_setval(i, (I + 1)),
        fail
    ; !),
    true.
:- initialization(main, main).
