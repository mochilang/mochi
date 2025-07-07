:- style_check(-singleton).
main :-
    Prefix = "fore",
    S1 = "forest",
    % unsupported: postfix ops not supported
    S2 = "desert",
    % unsupported: postfix ops not supported
    true.
:- initialization(main, main).
