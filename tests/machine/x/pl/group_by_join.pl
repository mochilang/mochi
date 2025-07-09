:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [Id-1, Name-"Alice"]),
    dict_create(_V1, map, [Id-2, Name-"Bob"]),
    Customers = [_V0, _V1],
    dict_create(_V2, map, [Id-100, CustomerId-1]),
    dict_create(_V3, map, [Id-101, CustomerId-1]),
    dict_create(_V4, map, [Id-102, CustomerId-2]),
    Orders = [_V2, _V3, _V4],
    get_item(O, 'customerId', _V5),
    get_item(C, 'id', _V6),
    get_item(G, 'key', _V7),
    length(G, _V8),
    dict_create(_V9, map, [Name-_V7, Count-_V8]),
    findall(_V9, (member(O, Orders), member(C, Customers), (_V5 == _V6), true), _V10),
    Stats = _V10,
    writeln("--- Orders per customer ---"),
    catch(
        (
            member(S, Stats),
                catch(
                    (
                        get_item(S, 'name', _V11),
                        writeln(_V11),
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
