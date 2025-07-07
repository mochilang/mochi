:- style_check(-singleton).
main :-
    % unsupported: unsupported primary
    % unsupported: unsupported primary
    nb_setval(tmp, []),
    % unsupported: for without range not supported
    % unsupported: unsupported primary
    write(Result),
    nl,
    true.
:- initialization(main, main).
