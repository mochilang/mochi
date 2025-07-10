:- style_check(-singleton).
:- initialization(main, main).
main :-
    Data = [1, 2],
    (once((member(X, Data), (X == 1))) -> Flag = true ; Flag = false),
    write(Flag),
    nl,
    true.
