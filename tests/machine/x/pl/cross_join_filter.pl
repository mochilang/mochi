:- style_check(-singleton).
main :-
    Nums is [1, 2, 3],
    Letters is ["A", "B"],
    % unsupported: unsupported primary
    write("--- Even pairs ---"),
    nl,
    (member(P, Pairs),
        % unsupported: print with one arg supported
        true.
    :- initialization(main, main).
