:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [Name-"Laptop", Price-1500]),
    dict_create(_V1, map, [Name-"Smartphone", Price-900]),
    dict_create(_V2, map, [Name-"Tablet", Price-600]),
    dict_create(_V3, map, [Name-"Monitor", Price-300]),
    dict_create(_V4, map, [Name-"Keyboard", Price-100]),
    dict_create(_V5, map, [Name-"Mouse", Price-50]),
    dict_create(_V6, map, [Name-"Headphones", Price-200]),
    Products = [_V0, _V1, _V2, _V3, _V4, _V5, _V6],
    findall(P, (member(P, Products), true), _V7),
    Expensive = _V7,
    writeln("--- Top products (excluding most expensive) ---"),
    catch(
        (
            member(Item, Expensive),
                catch(
                    (
                        get_item(Item, 'name', _V8),
                        writeln(_V8),
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
