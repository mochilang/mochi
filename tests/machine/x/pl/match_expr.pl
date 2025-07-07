:- style_check(-singleton).
main :-
    X = 2,
    % unsupported: unsupported primary
    write(Label),
    nl,
    true.
:- initialization(main, main).
