:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [id-1, name-"Alice"]),
    dict_create(_V1, map, [id-2, name-"Bob"]),
    Customers = [_V0, _V1],
    dict_create(_V2, map, [id-100, customerid-1]),
    dict_create(_V3, map, [id-101, customerid-1]),
    dict_create(_V4, map, [id-102, customerid-2]),
    Orders = [_V2, _V3, _V4],
    findall(_V10, (member(O, Orders), member(C, Customers), get_item(O, 'customerid', _V5), get_item(C, 'id', _V6), (_V5 == _V6), true, get_item(G, 'key', _V7), length(G, _V8), dict_create(_V9, map, [name-_V7, count-_V8]), _V10 = _V9), _V11),
    Stats = _V11,
    write("--- Orders per customer ---"),
    nl,
    catch(
        (
            member(S, Stats),
                catch(
                    (
                        get_item(S, 'name', _V12),
                        write(_V12),
                        write(' '),
                        write("orders:"),
                        write(' '),
                        get_item(S, 'count', _V13),
                        write(_V13),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
