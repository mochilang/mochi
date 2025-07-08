:- style_check(-singleton).
boom(, _V0) :-
    write("boom"),
    nl,
    _V0 = true.

main :-
    write((((1 < 2), (2 < 3)), (3 < 4))),
    nl,
    % unsupported: unsupported primary
    % unsupported: unsupported primary
    true.
:- initialization(main, main).
