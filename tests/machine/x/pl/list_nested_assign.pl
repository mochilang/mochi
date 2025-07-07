:- style_check(-singleton).
main :-
    nb_setval(matrix, [[1, 2], [3, 4]]),
    nb_setval(matrix, 5),
    % unsupported: postfix ops not supported
    true.
:- initialization(main, main).
