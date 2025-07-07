:- style_check(-singleton).
main :-
    Xs = [10, 20, 30],
    % unsupported: postfix ops not supported
    true.
:- initialization(main, main).
