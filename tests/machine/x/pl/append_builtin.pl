:- style_check(-singleton).
main :-
    A is [1, 2],
    % unsupported: unsupported primary
    true.
:- initialization(main, main).
