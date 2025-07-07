:- style_check(-singleton).
main :-
    X = 2,
    % unsupported: unsupported primary
    write(Label),
    nl,
    Day = "sun",
    % unsupported: unsupported primary
    write(Mood),
    nl,
    Ok = true,
    % unsupported: unsupported primary
    write(Status),
    nl,
    % unsupported: unsupported statement
    % unsupported: unsupported primary
    % unsupported: unsupported primary
    true.
:- initialization(main, main).
