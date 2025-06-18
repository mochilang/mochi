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
