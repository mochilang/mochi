:- style_check(-singleton).
main :-
    % unsupported: unsupported primary
    nb_setval(scores, 2),
    % unsupported: postfix ops not supported
    true.
:- initialization(main, main).
