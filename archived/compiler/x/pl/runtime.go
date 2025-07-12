//go:build archived

package plcode

const helperSlice = "slice(Str, I, J, Out) :-\n" +
	"    string(Str), !,\n" +
	"    Len is J - I,\n" +
	"    sub_string(Str, I, Len, _, Out).\n" +
	"slice(List, I, J, Out) :-\n" +
	"    length(Prefix, I),\n" +
	"    append(Prefix, Rest, List),\n" +
	"    Len is J - I,\n" +
	"    length(Out, Len),\n" +
	"    append(Out, _, Rest).\n\n"

const helperToList = "to_list(Str, L) :-\n" +
	"    string(Str), !,\n" +
	"    string_chars(Str, L).\n" +
	"to_list(L, L).\n\n"

const helperGetItem = "get_item(Container, Key, Val) :-\n" +
	"    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).\n" +
	"get_item(Container, Index, Val) :-\n" +
	"    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).\n" +
	"get_item(List, Index, Val) :- nth0(Index, List, Val).\n\n"

const helperSetItem = "set_item(Container, Key, Val, Out) :-\n" +
	"    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), put_dict(A, Container, Val, Out).\n" +
	"set_item(List, Index, Val, Out) :-\n" +
	"    nth0(Index, List, _, Rest),\n" +
	"    nth0(Index, Out, Val, Rest).\n\n"

const helperContains = "contains(Container, Item, Res) :-\n" +
	"    is_dict(Container), !, (get_dict(Item, Container, _) -> Res = true ; Res = false).\n" +
	"contains(List, Item, Res) :-\n" +
	"    string(List), !, string_chars(List, Chars), (member(Item, Chars) -> Res = true ; Res = false).\n" +
	"contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).\n\n"

const helperInput = "input(S) :- read_line_to_string(user_input, S).\n\n"

const helperStartsWith = "starts_with(Str, Prefix, Res) :-\n" +
	"    string(Str), !, (sub_string(Str, 0, _, _, Prefix) -> Res = true ; Res = false).\n" +
	"starts_with(_, _, false).\n\n"

const helperCount = "count(V, R) :-\n" +
	"    is_dict(V), !, get_dict('Items', V, Items), length(Items, R).\n" +
	"count(V, R) :-\n" +
	"    string(V), !, string_chars(V, C), length(C, R).\n" +
	"count(V, R) :-\n" +
	"    is_list(V), !, length(V, R).\n" +
	"count(_, _) :- throw(error('count expects list or group')).\n\n"

const helperAvg = "avg(V, R) :-\n" +
	"    is_dict(V), !, get_dict('Items', V, Items), avg_list(Items, R).\n" +
	"avg(V, R) :-\n" +
	"    is_list(V), !, avg_list(V, R).\n" +
	"avg(_, _) :- throw(error('avg expects list or group')).\n" +
	"avg_list([], 0).\n" +
	"avg_list(L, R) :- sum_list(L, S), length(L, N), N > 0, R is S / N.\n\n"

const helperSum = "sum(V, R) :-\n" +
	"    is_dict(V), !, get_dict('Items', V, Items), sum_list(Items, R).\n" +
	"sum(V, R) :-\n" +
	"    is_list(V), !, sum_list(V, R).\n" +
	"sum(_, _) :- throw(error('sum expects list or group')).\n\n"

const helperMin = "min(V, R) :-\n" +
	"    is_dict(V), !, get_dict('Items', V, Items), min_list(Items, R).\n" +
	"min(V, R) :-\n" +
	"    is_list(V), !, min_list(V, R).\n" +
	"min(_, _) :- throw(error('min expects list or group')).\n\n"

const helperJSON = ":- use_module(library(http/json)).\n" +
	"json(V) :- json_write_dict(current_output, V), nl.\n\n"

const helperExpect = "expect(Cond) :- (Cond -> true ; throw(error('expect failed'))).\n\n"

const helperUnionAll = "union_all(A, B, R) :- append(A, B, R).\n\n"

const helperUnion = "union(A, B, R) :- append(A, B, C), list_to_set(C, R).\n\n"

const helperExcept = "except([], _, []).\n" +
	"except([H|T], B, R) :- memberchk(H, B), !, except(T, B, R).\n" +
	"except([H|T], B, [H|R]) :- except(T, B, R).\n\n"

const helperIntersect = "intersect(A, B, R) :- intersect(A, B, [], R).\n" +
	"intersect([], _, Acc, R) :- reverse(Acc, R).\n" +
	"intersect([H|T], B, Acc, R) :- memberchk(H, B), \\+ memberchk(H, Acc), !, intersect(T, B, [H|Acc], R).\n" +
	"intersect([_|T], B, Acc, R) :- intersect(T, B, Acc, R).\n\n"

const helperMapKeys = "map_keys(Dict, Keys) :-\n" +
	"    dict_pairs(Dict, _, Pairs),\n" +
	"    findall(K, member(K-_, Pairs), Keys).\n\n"

const helperDatasetFilter = "dataset_filter([], _, []).\n" +
	"dataset_filter([H|T], Pred, [H|R]) :- call(Pred, H, Res), Res, !, dataset_filter(T, Pred, R).\n" +
	"dataset_filter([_|T], Pred, R) :- dataset_filter(T, Pred, R).\n\n"

const helperDatasetPaginate = "dataset_paginate(List, Skip, Take, Out) :-\n" +
	"    length(List, Len),\n" +
	"    (Skip >= Len -> Out = [] ;\n" +
	"    Start is max(Skip, 0),\n" +
	"    (Take < 0 -> End = Len ; Temp is Start + Take, (Temp > Len -> End = Len ; End = Temp)),\n" +
	"    slice(List, Start, End, Out)).\n\n"

const helperGroupBy = "group_insert(Key, Item, [], [_{key:Key, 'Items':[Item]}]).\n" +
	"group_insert(Key, Item, [G|Gs], [NG|Gs]) :- get_dict(key, G, Key), !, get_dict('Items', G, Items), append(Items, [Item], NItems), put_dict('Items', G, NItems, NG).\n" +
	"group_insert(Key, Item, [G|Gs], [G|Rs]) :- group_insert(Key, Item, Gs, Rs).\n" +
	"group_pairs([], Acc, Res) :- reverse(Acc, Res).\n" +
	"group_pairs([K-V|T], Acc, Res) :- group_insert(K, V, Acc, Acc1), group_pairs(T, Acc1, Res).\n" +
	"group_by(List, Fn, Groups) :- findall(K-V, (member(V, List), call(Fn, V, K)), Pairs), group_pairs(Pairs, [], Groups).\n\n"

const helperLoad = ":- use_module(library(http/json)).\n" +
	"load_data(Path, Opts, Rows) :-\n" +
	"    (is_dict(Opts), get_dict(format, Opts, Fmt) -> true ; Fmt = 'json'),\n" +
	"    (Path == '' ; Path == '-' -> read_string(user_input, _, Text) ; read_file_to_string(Path, Text, [])),\n" +
	"    (Fmt == 'jsonl' ->\n" +
	"        split_string(Text, '\\n', ' \\t\\r', Lines0),\n" +
	"        exclude(=(\"\"), Lines0, Lines),\n" +
	"        findall(D, (member(L, Lines), open_string(L, S), json_read_dict(S, D), close(S)), Rows)\n" +
	"    ;\n" +
	"        open_string(Text, S), json_read_dict(S, Data), close(S),\n" +
	"        (is_list(Data) -> Rows = Data ; Rows = [Data])\n" +
	"    ).\n\n"

const helperSave = ":- use_module(library(http/json)).\n" +
	"save_data(Rows, Path, Opts) :-\n" +
	"    (is_dict(Opts), get_dict(format, Opts, Fmt) -> true ; Fmt = 'json'),\n" +
	"    (Path == '' ; Path == '-' -> Out = current_output ; open(Path, write, Out)),\n" +
	"    (Fmt == 'jsonl' ->\n" +
	"        forall(member(R, Rows), (json_write_dict(Out, R), nl(Out)))\n" +
	"    ;\n" +
	"        json_write_dict(Out, Rows)\n" +
	"    ),\n" +
	"    (Out == current_output -> flush_output(Out) ; close(Out)).\n\n"

const helperFetch = ":- use_module(library(http/json)).\n" +
	":- use_module(library(process)).\n" +
	"fetch_data(URL, _Opts, Data) :-\n" +
	"    atom_string(U, URL),\n" +
	"    ( sub_atom(U, 0, 7, _, 'file://') ->\n" +
	"        sub_atom(U, 7, _, 0, Path),\n" +
	"        read_file_to_string(Path, Text, [])\n" +
	"    ;   process_create(path(curl), ['-s', U], [stdout(pipe(S))]),\n" +
	"        read_string(S, _, Text),\n" +
	"        close(S)\n" +
	"    ),\n" +
	"    atom_json_dict(Text, Data, []).\n\n"
