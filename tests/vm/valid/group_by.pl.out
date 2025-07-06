:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


count(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), length(Items, R).
count(V, R) :-
    string(V), !, string_chars(V, C), length(C, R).
count(V, R) :-
    is_list(V), !, length(V, R).
count(_, _) :- throw(error('count expects list or group')).


avg(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), avg_list(Items, R).
avg(V, R) :-
    is_list(V), !, avg_list(V, R).
avg(_, _) :- throw(error('avg expects list or group')).
avg_list([], 0).
avg_list(L, R) :- sum_list(L, S), length(L, N), N > 0, R is S / N.


group_insert(Key, Item, [], [_{key:Key, 'Items':[Item]}]).
group_insert(Key, Item, [G|Gs], [NG|Gs]) :- get_dict(key, G, Key), !, get_dict('Items', G, Items), append(Items, [Item], NItems), put_dict('Items', G, NItems, NG).
group_insert(Key, Item, [G|Gs], [G|Rs]) :- group_insert(Key, Item, Gs, Rs).
group_pairs([], Acc, Res) :- reverse(Acc, Res).
group_pairs([K-V|T], Acc, Res) :- group_insert(K, V, Acc, Acc1), group_pairs(T, Acc1, Res).
group_by(List, Fn, Groups) :- findall(K-V, (member(V, List), call(Fn, V, K)), Pairs), group_pairs(Pairs, [], Groups).


        p__lambda0(Person, Res) :-
        get_dict(city, Person, _V6),
        Res = _V6.

        main :-
    dict_create(_V0, map, [name-"Alice", age-30, city-"Paris"]),
    dict_create(_V1, map, [name-"Bob", age-15, city-"Hanoi"]),
    dict_create(_V2, map, [name-"Charlie", age-65, city-"Paris"]),
    dict_create(_V3, map, [name-"Diana", age-45, city-"Hanoi"]),
    dict_create(_V4, map, [name-"Eve", age-70, city-"Paris"]),
    dict_create(_V5, map, [name-"Frank", age-22, city-"Hanoi"]),
    People = [_V0, _V1, _V2, _V3, _V4, _V5],
    to_list(People, _V17),
    group_by(_V17, p__lambda0, _V18),
    findall(_V19, (member(G, _V18), get_dict(key, G, _V7), get_dict('Items', G, _V8), count(_V8, _V9), get_dict('Items', G, _V10), to_list(_V10, _V12), findall(_V13, (member(P, _V12), get_dict(age, P, _V11), _V13 = _V11), _V14), avg(_V14, _V15), dict_create(_V16, map, [city-_V7, count-_V9, avg_age-_V15]), _V19 = _V16), _V20),
    Stats = _V20,
    write("--- People grouped by city ---"),
    nl,
    to_list(Stats, _V21),
    catch(
        (
            member(S, _V21),
            catch(
                (
                    get_dict(city, S, _V22),
                    write(_V22),
                    write(' '),
                    write(": count ="),
                    write(' '),
                    get_dict(count, S, _V23),
                    write(_V23),
                    write(' '),
                    write(", avg_age ="),
                    write(' '),
                    get_dict(avg_age, S, _V24),
                    write(_V24),
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
