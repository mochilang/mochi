:- style_check(-singleton).
main :-
    Data is [1, 2],
    % unsupported: unsupported primary
    write(Flag),
    nl,
    true.
:- initialization(main, main).
