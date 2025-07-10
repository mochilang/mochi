:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [name-"Alice", age-30, city-"Paris"]),
    dict_create(_V1, map, [name-"Bob", age-15, city-"Hanoi"]),
    dict_create(_V2, map, [name-"Charlie", age-65, city-"Paris"]),
    dict_create(_V3, map, [name-"Diana", age-45, city-"Hanoi"]),
    dict_create(_V4, map, [name-"Eve", age-70, city-"Paris"]),
    dict_create(_V5, map, [name-"Frank", age-22, city-"Hanoi"]),
    People = [_V0, _V1, _V2, _V3, _V4, _V5],
    findall(_V15, (member(Person, People), true, get_item(G, 'key', _V6), length(G, _V7), findall(_V9, (member(P, G), true, get_item(P, 'age', _V8), _V9 = _V8), _V10), sum_list(_V10, _V11), length(_V10, _V12), _V12 > 0, _V13 is _V11 / _V12, dict_create(_V14, map, [city-_V6, count-_V7, avg_age-_V13]), _V15 = _V14), _V16),
    Stats = _V16,
    write("--- People grouped by city ---"),
    nl,
    catch(
        (
            member(S, Stats),
                catch(
                    (
                        get_item(S, 'city', _V17),
                        write(_V17),
                        write(' '),
                        write(": count ="),
                        write(' '),
                        get_item(S, 'count', _V18),
                        write(_V18),
                        write(' '),
                        write(", avg_age ="),
                        write(' '),
                        get_item(S, 'avg_age', _V19),
                        write(_V19),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
