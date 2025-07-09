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
    dict_create(_V2, map, [Id-3, Name-"Charlie"]),
    Customers = [_V0, _V1, _V2],
    dict_create(_V3, map, [Id-100, CustomerId-1, Total-250]),
    dict_create(_V4, map, [Id-101, CustomerId-2, Total-125]),
    dict_create(_V5, map, [Id-102, CustomerId-1, Total-300]),
    dict_create(_V6, map, [Id-103, CustomerId-4, Total-80]),
    Orders = [_V3, _V4, _V5, _V6],
    get_item(O, 'customerId', _V7),
    get_item(C, 'id', _V8),
    get_item(O, 'id', _V9),
    get_item(C, 'name', _V10),
    get_item(O, 'total', _V11),
    dict_create(_V12, map, [OrderId-_V9, CustomerName-_V10, Total-_V11]),
    findall(_V12, (member(O, Orders), member(C, Customers), (_V7 == _V8), true), _V13),
    Result = _V13,
    writeln("--- Orders with customer info ---"),
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
