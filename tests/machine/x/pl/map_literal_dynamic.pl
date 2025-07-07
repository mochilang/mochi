:- style_check(-singleton).
main :-
    nb_setval(x, 3),
    nb_setval(y, 4),
    % unsupported: unsupported primary
    % unsupported: print with one arg supported
    true.
:- initialization(main, main).
