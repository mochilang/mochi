:- style_check(-singleton).
slice(Str, I, J, Out) :-
    string(Str), !,
    Len is J - I,
    sub_string(Str, I, Len, _, Out).
slice(List, I, J, Out) :-
    length(Prefix, I),
    append(Prefix, Rest, List),
    Len is J - I,
    length(Out, Len),
    append(Out, _, Rest).


to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


        main :-
    dict_create(_V0, map, [name-"Laptop", price-1500]),
    dict_create(_V1, map, [name-"Smartphone", price-900]),
    dict_create(_V2, map, [name-"Tablet", price-600]),
    dict_create(_V3, map, [name-"Monitor", price-300]),
    dict_create(_V4, map, [name-"Keyboard", price-100]),
    dict_create(_V5, map, [name-"Mouse", price-50]),
    dict_create(_V6, map, [name-"Headphones", price-200]),
    Products = [_V0, _V1, _V2, _V3, _V4, _V5, _V6],
    to_list(Products, _V9),
    findall(_V11-_V10, (member(P, _V9), get_dict(price, P, _V7), _V8 is -(_V7), _V11 = _V8, _V10 = P), _V12),
    keysort(_V12, _V13),
    findall(V, member(_-V, _V13), _V14),
    length(_V14, _V15),
    _V16 is 1 + 3,
    slice(_V14, 1, _V16, _V17),
    Expensive = _V17,
    write("--- Top products (excluding most expensive) ---"),
    nl,
    to_list(Expensive, _V18),
    catch(
        (
            member(Item, _V18),
            catch(
                (
                    get_dict(name, Item, _V19),
                    write(_V19),
                    write(' '),
                    write("costs $"),
                    write(' '),
                    get_dict(price, Item, _V20),
                    write(_V20),
                    nl,
                    true
                ), continue, true),
                fail
                ;
                true
            )
            , break, true),
            true
        .
:- initialization(main, main).
