:- style_check(-singleton).
main :-
    % unsupported: var without init
    write(X),
    nl,
    true.
:- initialization(main, main).
