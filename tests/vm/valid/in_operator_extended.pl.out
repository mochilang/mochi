:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


contains(Container, Item, Res) :-
    is_dict(Container), !, (get_dict(Item, Container, _) -> Res = true ; Res = false).
contains(List, Item, Res) :-
    string(List), !, string_chars(List, Chars), (member(Item, Chars) -> Res = true ; Res = false).
contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).


    main :-
    Xs = [1, 2, 3],
    to_list(Xs, _V1),
    findall(X, (member(X, _V1), _V2 is X mod 2, _V2 = 1), _V3),
    findall(_V4, (member(X, _V3), _V4 = X), _V5),
    Ys = _V5,
    contains(Ys, 1, _V6),
    write(_V6),
    nl,
    contains(Ys, 2, _V7),
    write(_V7),
    nl,
    dict_create(_V8, map, [a-1]),
    M = _V8,
    contains(M, "a", _V9),
    write(_V9),
    nl,
    contains(M, "b", _V10),
    write(_V10),
    nl,
    S = "hello",
    contains(S, "ell", _V11),
    write(_V11),
    nl,
    contains(S, "foo", _V12),
    write(_V12),
    nl
    .
:- initialization(main, main).
