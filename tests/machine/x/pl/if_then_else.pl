:- style_check(-singleton).
main :-
    X = 12,
    % unsupported: unsupported primary
    write(Msg),
    nl,
    true.
:- initialization(main, main).
