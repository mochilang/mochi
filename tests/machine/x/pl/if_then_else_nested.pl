:- style_check(-singleton).
main :-
    X = 8,
    % unsupported: unsupported primary
    write(Msg),
    nl,
    true.
:- initialization(main, main).
