:- style_check(-singleton).
main :-
    nb_setval(nums, [1, 2]),
    nb_setval(nums, 3),
    % unsupported: postfix ops not supported
    true.
:- initialization(main, main).
