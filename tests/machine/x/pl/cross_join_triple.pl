:- style_check(-singleton).
main :-
    Nums = [1, 2],
    Letters = ["A", "B"],
    Bools = [true, false],
    % unsupported: unsupported primary
    write("--- Cross Join of three lists ---"),
    nl,
    % unsupported: for without range not supported
    true.
:- initialization(main, main).
