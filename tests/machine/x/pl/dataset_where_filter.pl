:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [name-"Alice", age-30]),
    dict_create(_V1, map, [name-"Bob", age-15]),
    dict_create(_V2, map, [name-"Charlie", age-65]),
    dict_create(_V3, map, [name-"Diana", age-45]),
    People = [_V0, _V1, _V2, _V3],
    findall(_V9, (member(Person, People), get_item(Person, 'age', _V4), (_V4 @>= 18), get_item(Person, 'name', _V5), get_item(Person, 'age', _V6), get_item(Person, 'age', _V7), dict_create(_V8, map, [name-_V5, age-_V6, is_senior-(_V7 @>= 60)]), _V9 = _V8), _V10),
    Adults = _V10,
    writeln("--- Adults ---"),
    catch(
        (
            member(Person, Adults),
                catch(
                    (
                        get_item(Person, 'name', _V11),
                        write(_V11),
                        write(' '),
                        write("is"),
                        write(' '),
                        get_item(Person, 'age', _V12),
                        write(_V12),
                        write(' '),
                        get_item(Person, 'is_senior', _V13),
                        (_V13 -> _V14 = " (senior)" ; _V14 = ""),
                        write(_V14),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
