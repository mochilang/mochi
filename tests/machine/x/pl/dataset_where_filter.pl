:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [Name-"Alice", Age-30]),
    dict_create(_V1, map, [Name-"Bob", Age-15]),
    dict_create(_V2, map, [Name-"Charlie", Age-65]),
    dict_create(_V3, map, [Name-"Diana", Age-45]),
    People = [_V0, _V1, _V2, _V3],
    get_item(Person, 'age', _V4),
    get_item(Person, 'name', _V5),
    get_item(Person, 'age', _V6),
    get_item(Person, 'age', _V7),
    dict_create(_V8, map, [Name-_V5, Age-_V6, Is_senior-(_V7 >= 60)]),
    findall(_V8, (member(Person, People), (_V4 >= 18)), _V9),
    Adults = _V9,
    writeln("--- Adults ---"),
    catch(
        (
            member(Person, Adults),
                catch(
                    (
                        get_item(Person, 'name', _V10),
                        writeln(_V10),
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
