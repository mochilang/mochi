:- style_check(-singleton).
main :-
    Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9],
    % unsupported: for without range not supported
    true.
:- initialization(main, main).
