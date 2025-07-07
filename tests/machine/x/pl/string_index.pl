:- style_check(-singleton).
main :-
    S = "mochi",
    % unsupported: postfix ops not supported
    true.
:- initialization(main, main).
