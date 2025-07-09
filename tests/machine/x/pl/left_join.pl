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
    dict_create(_V2, map, [Id-100, CustomerId-1, Total-250]),
    dict_create(_V3, map, [Id-101, CustomerId-3, Total-80]),
    Orders = [_V2, _V3],
    get_item(O, 'customerId', _V4),
    get_item(C, 'id', _V5),
    get_item(O, 'id', _V6),
    get_item(O, 'total', _V7),
    dict_create(_V8, map, [OrderId-_V6, Customer-C, Total-_V7]),
    findall(_V8, (member(O, Orders), member(C, Customers), (_V4 == _V5), true), _V9),
    Result = _V9,
    writeln("--- Left Join ---"),
    catch(
        (
            member(Entry, Result),
                catch(
                    (
                        writeln("Order"),
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
