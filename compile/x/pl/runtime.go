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
