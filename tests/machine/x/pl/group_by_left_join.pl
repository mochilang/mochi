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
    dict_create(_V2, map, [id-3, name-"Charlie"]),
    Customers = [_V0, _V1, _V2],
    dict_create(_V3, map, [id-100, customerid-1]),
    dict_create(_V4, map, [id-101, customerid-1]),
    dict_create(_V5, map, [id-102, customerid-2]),
    Orders = [_V3, _V4, _V5],
    findall(_V15, (member(C, Customers), findall(O, (member(O, Orders), (_V6 == _V7)), _V8), (_V8 = [] -> O = nil; member(O, _V8)), get_item(O, 'customerId', _V6), get_item(C, 'id', _V7), (_V6 == _V7), true, get_item(G, 'key', _V9), findall(_V11, (member(R, G), get_item(R, 'o', _V10), _V10, _V11 = R), _V12), length(_V12, _V13), dict_create(_V14, map, [name-_V9, count-_V13]), _V15 = _V14), _V16),
    Stats = _V16,
    write("--- Group Left Join ---"),
    nl,
    catch(
        (
            member(S, Stats),
                catch(
                    (
                        get_item(S, 'name', _V17),
                        write(_V17),
                        write(' '),
                        write("orders:"),
                        write(' '),
                        get_item(S, 'count', _V18),
                        write(_V18),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
