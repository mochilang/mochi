:- style_check(-singleton).
main :-
    % unsupported: unsupported statement
    write("ok"),
    nl,
    true.
:- initialization(main, main).
