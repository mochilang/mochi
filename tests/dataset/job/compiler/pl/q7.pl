% Generated by Mochi compiler v0.10.25 on 2025-07-13T12:57:15Z
:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

contains(Container, Item, Res) :-
    is_dict(Container), !, (string(Item) -> atom_string(A, Item) ; A = Item), (get_dict(A, Container, _) -> Res = true ; Res = false).
contains(List, Item, Res) :-
    string(List), !, (sub_string(List, _, _, _, Item) -> Res = true ; Res = false).
contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).

starts_with(Str, Prefix, Res) :-
    string(Str), !, (sub_string(Str, 0, _, _, Prefix) -> Res = true ; Res = false).

:- use_module(library(http/json)).
load_data(Path, Opts, Rows) :-
    (is_dict(Opts), get_dict(format, Opts, Fmt) -> true ; Fmt = 'json'),
    (Path == '' ; Path == '-' -> read_string(user_input, _, Text) ; read_file_to_string(Path, Text, [])),
    (Fmt == 'jsonl' ->
        split_string(Text, '\n', ' \t\r', Lines0),
        exclude(=(''), Lines0, Lines),
        findall(D, (member(L, Lines), open_string(L, S), json_read_dict(S, D), close(S)), Rows)
    ;
        open_string(Text, S), json_read_dict(S, Data), close(S),
        (is_list(Data) -> Rows = Data ; Rows = [Data])
    ).

expect(Cond) :- (Cond -> true ; throw(error('expect failed'))).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [person_id-1, name-"Anna Mae"]),
    dict_create(_V1, map, [person_id-2, name-"Chris"]),
    Aka_name = [_V0, _V1],
    dict_create(_V2, map, [person_id-1, movie_id-10]),
    dict_create(_V3, map, [person_id-2, movie_id-20]),
    Cast_info = [_V2, _V3],
    dict_create(_V4, map, [id-1, info-"mini biography"]),
    dict_create(_V5, map, [id-2, info-"trivia"]),
    Info_type = [_V4, _V5],
    dict_create(_V6, map, [id-1, link-"features"]),
    dict_create(_V7, map, [id-2, link-"references"]),
    Link_type = [_V6, _V7],
    dict_create(_V8, map, [linked_movie_id-10, link_type_id-1]),
    dict_create(_V9, map, [linked_movie_id-20, link_type_id-2]),
    Movie_link = [_V8, _V9],
    dict_create(_V10, map, [id-1, name-"Alan Brown", name_pcode_cf-"B", gender-"m"]),
    dict_create(_V11, map, [id-2, name-"Zoe", name_pcode_cf-"Z", gender-"f"]),
    Name = [_V10, _V11],
    dict_create(_V12, map, [person_id-1, info_type_id-1, note-"Volker Boehm"]),
    dict_create(_V13, map, [person_id-2, info_type_id-1, note-"Other"]),
    Person_info = [_V12, _V13],
    dict_create(_V14, map, [id-10, title-"Feature Film", production_year-1990]),
    dict_create(_V15, map, [id-20, title-"Late Film", production_year-2000]),
    Title = [_V14, _V15],
    findall(_V54, (member(An, Aka_name), member(N, Name), get_item(N, 'id', _V16), get_item(An, 'person_id', _V17), (_V16 == _V17), member(Pi, Person_info), get_item(Pi, 'person_id', _V18), get_item(An, 'person_id', _V19), (_V18 == _V19), member(It, Info_type), get_item(It, 'id', _V20), get_item(Pi, 'info_type_id', _V21), (_V20 == _V21), member(Ci, Cast_info), get_item(Ci, 'person_id', _V22), get_item(N, 'id', _V23), (_V22 == _V23), member(T, Title), get_item(T, 'id', _V24), get_item(Ci, 'movie_id', _V25), (_V24 == _V25), member(Ml, Movie_link), get_item(Ml, 'linked_movie_id', _V26), get_item(T, 'id', _V27), (_V26 == _V27), member(Lt, Link_type), get_item(Lt, 'id', _V28), get_item(Ml, 'link_type_id', _V29), (_V28 == _V29), get_item(An, 'name', _V30), contains(_V30, "a", _V31), get_item(It, 'info', _V32), get_item(Lt, 'link', _V33), get_item(N, 'name_pcode_cf', _V34), get_item(N, 'name_pcode_cf', _V35), get_item(N, 'gender', _V36), get_item(N, 'gender', _V37), get_item(N, 'name', _V38), starts_with(_V38, "B", _V39), get_item(Pi, 'note', _V40), get_item(T, 'production_year', _V41), get_item(T, 'production_year', _V42), get_item(Pi, 'person_id', _V43), get_item(An, 'person_id', _V44), get_item(Pi, 'person_id', _V45), get_item(Ci, 'person_id', _V46), get_item(An, 'person_id', _V47), get_item(Ci, 'person_id', _V48), get_item(Ci, 'movie_id', _V49), get_item(Ml, 'linked_movie_id', _V50), ((((((((((((_V31, (_V32 == "mini biography")), (_V33 == "features")), (_V34 >= "A")), (_V35 =< "F")), ((_V36 == "m") ; ((_V37 == "f"), _V39))), (_V40 == "Volker Boehm")), (_V41 >= 1980)), (_V42 =< 1995)), (_V43 == _V44)), (_V45 == _V46)), (_V47 == _V48)), (_V49 == _V50)), get_item(N, 'name', _V51), get_item(T, 'title', _V52), dict_create(_V53, map, [person_name-_V51, movie_title-_V52]), _V54 = _V53), _V55),
    Rows = _V55,
    findall(_V57, (member(R, Rows), true, get_item(R, 'person_name', _V56), _V57 = _V56), _V58),
    min_list(_V58, _V59),
    findall(_V61, (member(R, Rows), true, get_item(R, 'movie_title', _V60), _V61 = _V60), _V62),
    min_list(_V62, _V63),
    dict_create(_V64, map, [of_person-_V59, biography_movie-_V63]),
    Result = [_V64],
    json_write_dict(current_output, Result), nl,
    true,
    dict_create(_V65, map, [of_person-"Alan Brown", biography_movie-"Feature Film"]),
    expect((Result == [_V65])),
    true.
