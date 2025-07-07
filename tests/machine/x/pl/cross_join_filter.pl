:- style_check(-singleton).
main :-
    Nums = [1, 2, 3],
    Letters = ["A", "B"],
    % unsupported: unsupported primary
    write("--- Even pairs ---"),
    nl,
    % unsupported: for without range not supported
    true.
:- initialization(main, main).
