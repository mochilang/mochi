:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


    main :-
    Data = [1, 2],
    to_list(Data, _V0),
    findall(X, (member(X, _V0), X = 1), _V1),
    findall(_V2, (member(X, _V1), _V2 = X), _V3),
    call(Exists, _V3, _V4),
    Flag = _V4,
    write(Flag),
    nl
    .
:- initialization(main, main).
