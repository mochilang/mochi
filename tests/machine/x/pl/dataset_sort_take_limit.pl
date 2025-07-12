:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [name-"Laptop", price-1500]),
    dict_create(_V1, map, [name-"Smartphone", price-900]),
    dict_create(_V2, map, [name-"Tablet", price-600]),
    dict_create(_V3, map, [name-"Monitor", price-300]),
    dict_create(_V4, map, [name-"Keyboard", price-100]),
    dict_create(_V5, map, [name-"Mouse", price-50]),
    dict_create(_V6, map, [name-"Headphones", price-200]),
    Products = [_V0, _V1, _V2, _V3, _V4, _V5, _V6],
    findall(_V7, (member(P, Products), true, _V7 = P), _V8),
    Expensive = _V8,
    writeln("--- Top products (excluding most expensive) ---"),
    catch(
        (
            member(Item, Expensive),
                catch(
                    (
                        get_item(Item, 'name', _V9),
                        write(_V9),
                        write(' '),
                        write("costs $"),
                        write(' '),
                        get_item(Item, 'price', _V10),
                        write(_V10),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
