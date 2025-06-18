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
	"    is_dict(Container), !, get_dict(Key, Container, Val).\n" +
	"get_item(Container, Index, Val) :-\n" +
	"    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).\n" +
	"get_item(List, Index, Val) :- nth0(Index, List, Val).\n\n"

const helperSetItem = "set_item(Container, Key, Val, Out) :-\n" +
	"    is_dict(Container), !, put_dict(Key, Container, Val, Out).\n" +
	"set_item(List, Index, Val, Out) :-\n" +
	"    nth0(Index, List, _, Rest),\n" +
	"    nth0(Index, Out, Val, Rest).\n\n"

const helperContains = "contains(Container, Item, Res) :-\n" +
	"    is_dict(Container), !, (get_dict(Item, Container, _) -> Res = true ; Res = false).\n" +
	"contains(List, Item, Res) :-\n" +
	"    string(List), !, string_chars(List, Chars), (member(Item, Chars) -> Res = true ; Res = false).\n" +
	"contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).\n\n"
