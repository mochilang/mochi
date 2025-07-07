:- style_check(-singleton).
main :-
    % unsupported: unsupported expression
    write(((1 + 2) * 3)),
    nl,
    % unsupported: unsupported expression
    write((2 * (3 + 1))),
    nl,
    true.
:- initialization(main, main).
