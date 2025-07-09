:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [Name-"Alice", Age-30, City-"Paris"]),
    dict_create(_V1, map, [Name-"Bob", Age-15, City-"Hanoi"]),
    dict_create(_V2, map, [Name-"Charlie", Age-65, City-"Paris"]),
    dict_create(_V3, map, [Name-"Diana", Age-45, City-"Hanoi"]),
    dict_create(_V4, map, [Name-"Eve", Age-70, City-"Paris"]),
    dict_create(_V5, map, [Name-"Frank", Age-22, City-"Hanoi"]),
    People = [_V0, _V1, _V2, _V3, _V4, _V5],
    get_item(G, 'key', _V6),
    length(G, _V7),
    get_item(P, 'age', _V8),
    findall(_V8, (member(P, G), true), _V9),
    sum_list(_V9, _V10),
    length(_V9, _V11),
    _V11 > 0,
    _V12 is _V10 / _V11,
    dict_create(_V13, map, [City-_V6, Count-_V7, Avg_age-_V12]),
    findall(_V13, (member(Person, People), true), _V14),
    Stats = _V14,
    writeln("--- People grouped by city ---"),
    catch(
        (
            member(S, Stats),
                catch(
                    (
                        get_item(S, 'city', _V15),
                        writeln(_V15),
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
