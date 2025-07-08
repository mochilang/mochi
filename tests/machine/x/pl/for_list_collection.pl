:- style_check(-singleton).
main :-
    (member(N, [1, 2, 3]),
        write(N),
        nl,
        fail
    ; true),
    true.
:- initialization(main, main).
