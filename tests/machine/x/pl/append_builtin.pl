:- style_check(-singleton).
main :-
    A = [1, 2],
    % unsupported: unsupported primary
    true.
:- initialization(main, main).
