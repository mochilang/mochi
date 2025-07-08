:- style_check(-singleton).
main :-
    Nums is [1, 2],
    Letters is ["A", "B"],
    Bools is [true, false],
    % unsupported: unsupported primary
    write("--- Cross Join of three lists ---"),
    nl,
    (member(C, Combos),
        % unsupported: print with one arg supported
        true.
    :- initialization(main, main).
