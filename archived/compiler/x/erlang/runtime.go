//go:build archived

package erlcode

import (
	"fmt"
	"strings"
)

// Runtime helper functions injected into generated programs.
func (c *Compiler) emitRuntime() {
	if c.needPrint {
		c.writeln("mochi_print(Args) ->")
		c.indent++
		c.writeln("Strs = [ mochi_format(A) || A <- Args ],")
		c.writeln("io:format(\"~s~n\", [lists:flatten(Strs)]).")
		c.indent--
		c.writeln("")
	}

	if c.needFormat {
		c.writeln("mochi_format(X) when is_integer(X) -> integer_to_list(X);")
		c.writeln("mochi_format(X) when is_float(X) -> float_to_list(X);")
		c.writeln("mochi_format(X) when is_list(X) -> X;")
		c.writeln("mochi_format(X) -> lists:flatten(io_lib:format(\"~p\", [X])).")
		c.writeln("")
	}

	if c.needCount {
		c.writeln("mochi_count(X) when is_list(X) -> length(X);")
		c.writeln("mochi_count(X) when is_map(X), maps:is_key('Items', X) -> length(maps:get('Items', X));")
		c.writeln("mochi_count(X) when is_map(X) -> maps:size(X);")
		c.writeln("mochi_count(X) when is_binary(X) -> byte_size(X);")
		c.writeln("mochi_count(_) -> erlang:error(badarg).")
		c.writeln("")
	}

	if c.needInput {
		c.writeln("mochi_input() ->")
		c.indent++
		c.writeln("case io:get_line(\"\") of")
		c.indent++
		c.writeln("eof -> \"\";")
		c.writeln("Line -> string:trim(Line)")
		c.indent--
		c.writeln("end.")
		c.indent--
		c.writeln("")
	}

	if c.needAvg {
		c.writeln("mochi_avg([]) -> 0;")
		c.writeln("mochi_avg(M) when is_map(M), maps:is_key('Items', M) -> mochi_avg(maps:get('Items', M));")
		c.writeln("mochi_avg(L) when is_list(L) ->")
		c.indent++
		c.writeln("Sum = lists:foldl(fun(X, Acc) ->")
		c.indent++
		c.writeln("case X of")
		c.indent++
		c.writeln("I when is_integer(I) -> Acc + I;")
		c.writeln("F when is_float(F) -> Acc + F;")
		c.writeln("_ -> erlang:error(badarg) end")
		c.indent--
		c.writeln("end, 0, L),")
		c.writeln("Sum / length(L);")
		c.indent--
		c.writeln("mochi_avg(_) -> erlang:error(badarg).")
		c.indent--
		c.writeln("")
	}

	if c.needSum {
		c.writeln("mochi_sum([]) -> 0;")
		c.writeln("mochi_sum(M) when is_map(M), maps:is_key('Items', M) -> mochi_sum(maps:get('Items', M));")
		c.writeln("mochi_sum(L) when is_list(L) ->")
		c.indent++
		c.writeln("lists:foldl(fun(X, Acc) ->")
		c.indent++
		c.writeln("case X of")
		c.indent++
		c.writeln("I when is_integer(I) -> Acc + I;")
		c.writeln("F when is_float(F) -> Acc + F;")
		c.writeln("_ -> erlang:error(badarg) end")
		c.indent--
		c.writeln("end, 0, L);")
		c.indent--
		c.writeln("mochi_sum(_) -> erlang:error(badarg).")
		c.indent--
		c.writeln("")
	}

	if c.needMin {
		c.writeln("mochi_min([]) -> 0;")
		c.writeln("mochi_min(M) when is_map(M) ->")
		c.indent++
		c.writeln("case maps:find('Items', M) of")
		c.indent++
		c.writeln("{ok, Items} -> mochi_min(Items);")
		c.writeln("error -> erlang:error(badarg)")
		c.indent--
		c.writeln("end;")
		c.indent--
		c.writeln("mochi_min(L) when is_list(L) -> lists:min(L);")
		c.writeln("mochi_min(_) -> erlang:error(badarg).")
		c.writeln("")
	}

	if c.needMax {
		c.writeln("mochi_max([]) -> 0;")
		c.writeln("mochi_max(M) when is_map(M) ->")
		c.indent++
		c.writeln("case maps:find('Items', M) of")
		c.indent++
		c.writeln("{ok, Items} -> mochi_max(Items);")
		c.writeln("error -> erlang:error(badarg)")
		c.indent--
		c.writeln("end;")
		c.indent--
		c.writeln("mochi_max(L) when is_list(L) -> lists:max(L);")
		c.writeln("mochi_max(_) -> erlang:error(badarg).")
		c.writeln("")
	}

	if c.needForeach {
		c.writeln("mochi_foreach(F, L) ->")
		c.indent++
		c.writeln("try mochi_foreach_loop(F, L) catch throw:mochi_break -> ok end.")
		c.indent--

		c.writeln("")
		c.writeln("mochi_foreach_loop(_, []) -> ok;")
		c.writeln("mochi_foreach_loop(F, [H|T]) ->")
		c.indent++
		c.writeln("try F(H) catch")
		c.indent++
		c.writeln("throw:mochi_continue -> ok;")
		c.writeln("throw:mochi_break -> throw(mochi_break)")
		c.indent--
		c.writeln("end,")
		c.writeln("mochi_foreach_loop(F, T).")
		c.indent--
	}

	if c.needGet {
		c.writeln("")
		c.writeln("mochi_get(L, I) when is_list(L), is_integer(I) ->")
		c.indent++
		c.writeln("N = length(L),")
		c.writeln("Idx = case I >= 0 of true -> I + 1; false -> N + I + 1 end,")
		c.writeln("lists:nth(Idx, L);")
		c.indent--
		c.writeln("mochi_get(M, K) when is_map(M) -> maps:get(K, M);")
		c.writeln("mochi_get(_, _) -> erlang:error(badarg).")
	}

	if c.needSlice {
		c.writeln("")
		c.writeln("mochi_slice(L, I, J) ->")
		c.indent++
		c.writeln("N = length(L),")
		c.writeln("Start0 = case I < 0 of true -> I + N; false -> I end,")
		c.writeln("End0 = case J < 0 of true -> J + N; false -> J end,")
		c.writeln("Start1 = case Start0 < 0 of true -> 0; false -> case Start0 > N of true -> N; false -> Start0 end end,")
		c.writeln("End1 = case End0 > N of true -> N; false -> case End0 < Start1 of true -> Start1; false -> End0 end end,")
		c.writeln("Len = End1 - Start1,")
		c.writeln("lists:sublist(lists:nthtail(Start1, L), Len).")
		c.indent--
	}

	if c.needIO {
		c.writeln("")
		c.writeln("mochi_load(Path, Opts) ->")
		c.indent++
		c.writeln("case file:read_file(Path) of")
		c.indent++
		c.writeln("{ok, Bin} ->")
		c.indent++
		c.writeln("Format = case Opts of")
		c.indent++
		c.writeln("undefined -> undefined;")
		c.writeln("O -> maps:get(format, O, undefined)")
		c.indent--
		c.writeln("end,")
		c.writeln("Ext = filename:extension(Path),")
		c.writeln("Text = binary_to_list(Bin),")
		c.writeln("Data0 = case {Format, Ext} of")
		c.indent++
		c.writeln("{\"json\", _} -> mochi_decode_json(Text);")
		c.writeln("{_, \".json\"} -> mochi_decode_json(Text);")
		c.writeln("{_, \".txt\"} -> Text;")
		c.writeln("_ -> binary_to_term(Bin)")
		c.indent--
		c.writeln("end,")
		c.writeln("Data1 = mochi_filter(Data0, Opts),")
		c.writeln("mochi_paginate(Data1, Opts);")
		c.indent--
		c.writeln("_ -> []")
		c.indent--
		c.writeln("end.")
		c.indent--

		c.writeln("")
		c.writeln("mochi_save(Data, Path, Opts) ->")
		c.indent++
		c.writeln("Format = case Opts of")
		c.indent++
		c.writeln("undefined -> undefined;")
		c.writeln("O -> maps:get(format, O, undefined)")
		c.indent--
		c.writeln("end,")
		c.writeln("Ext = filename:extension(Path),")
		c.writeln("Bin = case {Format, Ext} of")
		c.indent++
		c.writeln("{\"json\", _} -> list_to_binary(mochi_to_json(Data));")
		c.writeln("{_, \".json\"} -> list_to_binary(mochi_to_json(Data));")
		c.writeln("{_, \".txt\"} -> Data;")
		c.writeln("_ -> term_to_binary(Data)")
		c.indent--
		c.writeln("end,")
		c.writeln("ok = file:write_file(Path, Bin).")
		c.indent--

		c.writeln("")
		c.writeln("mochi_filter(Data, Opts) when Opts =:= undefined -> Data;")
		c.writeln("mochi_filter(Data, Opts) when is_list(Data) ->")
		c.indent++
		c.writeln("case maps:get(filter, Opts, undefined) of")
		c.indent++
		c.writeln("undefined -> Data;")
		c.writeln("Fun when is_function(Fun,1) -> [ X || X <- Data, Fun(X) ];")
		c.writeln("_ -> Data")
		c.indent--
		c.writeln("end;")
		c.indent--
		c.writeln("mochi_filter(Data, _) -> Data.")

		c.writeln("")
		c.writeln("mochi_paginate(Data, Opts) when Opts =:= undefined -> Data;")
		c.writeln("mochi_paginate(Data, Opts) when is_list(Data) ->")
		c.indent++
		c.writeln("Skip = maps:get(skip, Opts, 0),")
		c.writeln("Take = maps:get(take, Opts, -1),")
		c.writeln("Skipped = case Skip of")
		c.indent++
		c.writeln("N when is_integer(N), N > 0 -> lists:nthtail(N, Data);")
		c.writeln("_ -> Data")
		c.indent--
		c.writeln("end,")
		c.writeln("case Take of")
		c.indent++
		c.writeln("N when is_integer(N), N >= 0 -> lists:sublist(Skipped, N);")
		c.writeln("_ -> Skipped")
		c.indent--
		c.writeln("end;")
		c.indent--
		c.writeln("mochi_paginate(Data, _) -> Data.")

		c.writeln("")
		c.writeln("mochi_decode_json(Text) ->")
		c.indent++
		c.writeln("{Val, _} = mochi_json_value(string:trim(Text)),")
		c.writeln("Val.")
		c.indent--

		c.writeln("")
		c.writeln("mochi_json_value([] = S) -> {[], S};")
		c.writeln("mochi_json_value([${}|S]) -> mochi_json_object(S, #{});")
		c.writeln("mochi_json_value([$[|S]) -> mochi_json_array(S, []);")
		c.writeln("mochi_json_value([$\"|S]) ->")
		c.indent++
		c.writeln("{Str, R} = mochi_json_string(S, []),")
		c.writeln("{Str, mochi_skip_ws(R)}.")
		c.indent--
		c.writeln("mochi_json_value(S) ->")
		c.indent++
		c.writeln("{Num, R} = mochi_json_number(S),")
		c.writeln("{Num, mochi_skip_ws(R)}.")
		c.indent--

		c.writeln("")
		c.writeln("mochi_json_array([$]|S], Acc) -> {lists:reverse(Acc), mochi_skip_ws(S)};")
		c.writeln("mochi_json_array(S, Acc) ->")
		c.indent++
		c.writeln("{Val, R0} = mochi_json_value(mochi_skip_ws(S)),")
		c.writeln("R1 = mochi_skip_ws(R0),")
		c.writeln("case R1 of")
		c.indent++
		c.writeln("[$,|T] -> mochi_json_array(T, [Val|Acc]);")
		c.writeln("[$]|T] -> {lists:reverse([Val|Acc]), mochi_skip_ws(T)};")
		c.writeln("_ -> {lists:reverse([Val|Acc]), R1}")
		c.indent--
		c.writeln("end.")
		c.indent--

		c.writeln("")
		c.writeln("mochi_json_object([$}|S], Acc) -> {Acc, mochi_skip_ws(S)};")
		c.writeln("mochi_json_object(S, Acc) ->")
		c.indent++
		c.writeln("{Key, R0} = mochi_json_string(mochi_skip_ws(S), []),")
		c.writeln("R1 = mochi_skip_ws(R0),")
		c.writeln("[$:|R2] = R1,")
		c.writeln("{Val, R3} = mochi_json_value(mochi_skip_ws(R2)),")
		c.writeln("R4 = mochi_skip_ws(R3),")
		c.writeln("Acc1 = maps:put(Key, Val, Acc),")
		c.writeln("case R4 of")
		c.indent++
		c.writeln("[$,|T] -> mochi_json_object(T, Acc1);")
		c.writeln("[$}|T] -> {Acc1, mochi_skip_ws(T)};")
		c.writeln("_ -> {Acc1, R4}")
		c.indent--
		c.writeln("end.")
		c.indent--

		c.writeln("")
		c.writeln("mochi_json_string([$\\,C|S], Acc) -> mochi_json_string(S, [C|Acc]);")
		c.writeln("mochi_json_string([$\"|S], Acc) -> {lists:reverse(Acc), S};")
		c.writeln("mochi_json_string([C|S], Acc) -> mochi_json_string(S, [C|Acc]).")

		c.writeln("")
		c.writeln("mochi_json_number(S) ->")
		c.indent++
		c.writeln("{NumStr, Rest} = mochi_take_number(S, []),")
		c.writeln("case string:to_float(NumStr) of")
		c.indent++
		c.writeln("{error, _} -> {list_to_integer(NumStr), Rest};")
		c.writeln("{F, _} -> {F, Rest}")
		c.indent--
		c.writeln("end.")
		c.indent--

		c.writeln("")
		c.writeln("mochi_take_number([C|S], Acc) when C >= $0, C =< $9; C == $.; C == $-; C == $+ -> mochi_take_number(S, [C|Acc]);")
		c.writeln("mochi_take_number(S, Acc) -> {lists:reverse(Acc), S}.")

		c.writeln("")
		c.writeln("mochi_skip_ws([C|S]) when C =< $\\s -> mochi_skip_ws(S);")
		c.writeln("mochi_skip_ws(S) -> S.")
	}

	if c.needFetch {
		c.writeln("")
		c.writeln("mochi_fetch(Url, Opts) when Opts =:= undefined ->")
		c.indent++
		c.writeln("mochi_fetch(Url, #{});")
		c.indent--
		c.writeln("mochi_fetch(Url, Opts) ->")
		c.indent++
		c.writeln("application:ensure_all_started(inets),")
		c.writeln("Method0 = maps:get(method, Opts, get),")
		c.writeln("Method = case Method0 of")
		c.indent++
		c.writeln("M when is_atom(M) -> M;")
		c.writeln("M when is_list(M) -> list_to_atom(string:lowercase(M));")
		c.writeln("M when is_binary(M) -> list_to_atom(string:lowercase(binary_to_list(M)));")
		c.writeln("_ -> get")
		c.indent--
		c.writeln("end,")
		c.writeln("Query = maps:get(query, Opts, undefined),")
		c.writeln("Url1 = case Query of")
		c.indent++
		c.writeln("undefined -> Url;")
		c.writeln("Q ->")
		c.indent++
		c.writeln("Pairs = [ K ++ \"=\" ++ lists:flatten(io_lib:format(\"~p\", [V])) || {K,V} <- maps:to_list(Q) ],")
		c.writeln("Sep = case lists:member($?, Url) of true -> \"&\"; false -> \"?\" end,")
		c.writeln("Url ++ Sep ++ string:join(Pairs, \"&\")")
		c.indent--
		c.writeln("end,")
		c.indent--
		c.writeln("HeadersMap = maps:get(headers, Opts, #{}),")
		c.writeln("Headers = [ {K, case V of B when is_binary(V) -> binary_to_list(V); _ -> lists:flatten(io_lib:format(\"~p\", [V])) end} || {K,V} <- maps:to_list(HeadersMap) ],")
		c.writeln("BodyOpt = maps:get(body, Opts, undefined),")
		c.writeln("Req = case BodyOpt of")
		c.indent++
		c.writeln("undefined -> {Url1, Headers};")
		c.writeln("B -> {Url1, Headers, \"application/json\", list_to_binary(mochi_to_json(B))}")
		c.indent--
		c.writeln("end,")
		c.writeln("TimeoutOpt = maps:get(timeout, Opts, undefined),")
		c.writeln("HTTPOpts = case TimeoutOpt of")
		c.indent++
		c.writeln("undefined -> [];")
		c.writeln("T when is_integer(T) -> [{timeout, T * 1000}];")
		c.writeln("T when is_float(T) -> [{timeout, trunc(T * 1000)}];")
		c.writeln("_ -> []")
		c.indent--
		c.writeln("end,")
		c.writeln("case string:prefix(Url1, \"file://\") of")
		c.indent++
		c.writeln("true ->")
		c.indent++
		c.writeln("{ok, Bin} = file:read_file(string:substr(Url1, 8)),")
		c.writeln("mochi_decode_json(binary_to_list(Bin));")
		c.indent--
		c.writeln("_ ->")
		c.indent++
		c.writeln("case httpc:request(Method, Req, HTTPOpts, []) of")
		c.indent++
		c.writeln("{ok, {{_, 200, _}, _H, Body}} -> mochi_decode_json(binary_to_list(Body));")
		c.writeln("_ -> #{}")
		c.indent--
		c.writeln("end")
		c.indent--
		c.writeln("end.")
		c.indent--
		c.indent--
	}

	if c.needGenText {
		c.writeln("")
		c.writeln("mochi_gen_text(Prompt, _Model, _Params) -> Prompt.")
	}

	if c.needGenEmbed {
		c.writeln("")
		c.writeln("mochi_gen_embed(Text, _Model, _Params) -> [ float(C) || <<C>> <= Text ].")
	}

	if c.needGenStruct {
		c.writeln("")
		c.writeln("mochi_gen_struct(_Mod, _Prompt, _Model, _Params) -> #{}.")
	}

	if c.needGroupBy {
		c.writeln("")
		c.writeln("mochi_group_by(Src, KeyFun) ->")
		c.indent++
		c.writeln("{Groups, Order} = lists:foldl(fun(It, {G,O}) ->")
		c.indent++
		c.writeln("Key = KeyFun(It),")
		c.writeln("KS = lists:flatten(io_lib:format(\"~p\", [Key])),")
		c.writeln("case maps:get(KS, G, undefined) of")
		c.indent++
		c.writeln("undefined ->")
		c.indent++
		c.writeln("Group = #{key => Key, 'Items' => [It]},")
		c.writeln("{maps:put(KS, Group, G), O ++ [KS]};")
		c.indent--
		c.writeln("Group0 ->")
		c.indent++
		c.writeln("Items = maps:get('Items', Group0) ++ [It],")
		c.writeln("Group1 = maps:put('Items', Items, Group0),")
		c.writeln("{maps:put(KS, Group1, G), O}")
		c.indent--
		c.writeln("end")
		c.indent--
		c.writeln("end, {#{}, []}, Src),")
		c.writeln("[ maps:get(K, Groups) || K <- Order ].")
		c.indent--
	}

	if c.needLeftJoin {
		c.writeln("")
		c.writeln("mochi_left_join_item(A, B, Fun) ->")
		c.indent++
		c.writeln("Matches = [ {A, J} || J <- B, Fun(A, J) ],")
		c.writeln("case Matches of")
		c.indent++
		c.writeln("[] -> [{A, undefined}];")
		c.writeln("_ -> Matches")
		c.indent--
		c.writeln("end.")
		c.indent--

		c.writeln("")
		c.writeln("mochi_left_join(L, R, Fun) ->")
		c.indent++
		c.writeln("lists:flatmap(fun(X) -> mochi_left_join_item(X, R, Fun) end, L).")
		c.indent--
	}

	if c.needRightJoin {
		c.writeln("")
		c.writeln("mochi_right_join_item(B, A, Fun) ->")
		c.indent++
		c.writeln("Matches = [ {I, B} || I <- A, Fun(I, B) ],")
		c.writeln("case Matches of")
		c.indent++
		c.writeln("[] -> [{undefined, B}];")
		c.writeln("_ -> Matches")
		c.indent--
		c.writeln("end.")
		c.indent--

		c.writeln("")
		c.writeln("mochi_right_join(L, R, Fun) ->")
		c.indent++
		c.writeln("lists:flatmap(fun(Y) -> mochi_right_join_item(Y, L, Fun) end, R).")
		c.indent--
	}

	if c.needOuterJoin {
		c.writeln("")
		c.writeln("mochi_outer_join(L, R, Fun) ->")
		c.indent++
		c.writeln("Left = mochi_left_join(L, R, Fun),")
		c.writeln("Right = [ P || P = {undefined, _} <- mochi_right_join(L, R, Fun) ],")
		c.writeln("Left ++ Right.")
		c.indent--
	}

	if c.needSetOps {
		c.writeln("")
		c.writeln("mochi_union(A, B) -> sets:to_list(sets:union(sets:from_list(A), sets:from_list(B))).")
		c.writeln("")
		c.writeln("mochi_except(A, B) -> sets:to_list(sets:subtract(sets:from_list(A), sets:from_list(B))).")
		c.writeln("")
		c.writeln("mochi_intersect(A, B) -> sets:to_list(sets:intersection(sets:from_list(A), sets:from_list(B))).")
	}

	if c.needWhile {
		c.writeln("")
		c.writeln("mochi_while(Cond, Body) ->")
		c.indent++
		c.writeln("case Cond() of")
		c.indent++
		c.writeln("true ->")
		c.indent++
		c.writeln("try Body() catch")
		c.indent++
		c.writeln("throw:mochi_continue -> ok;")
		c.writeln("throw:mochi_break -> ok")
		c.indent--
		c.writeln("end,")
		c.writeln("mochi_while(Cond, Body);")
		c.indent--
		c.writeln("_ -> ok")
		c.indent--
		c.writeln("end.")
		c.indent--
	}

	if c.needJSON {
		c.writeln("")
		c.writeln("mochi_escape_json([]) -> [];")
		c.writeln("mochi_escape_json([H|T]) ->")
		c.indent++
		c.writeln("E = case H of")
		c.indent++
		c.writeln("$\\\\ -> \"\\\\\\\\\";")
		c.writeln("$\" -> \"\\\\\"\";")
		c.writeln("_ -> [H]")
		c.indent--
		c.writeln("end,")
		c.writeln("E ++ mochi_escape_json(T).")
		c.indent--

		c.writeln("")
		c.writeln(`mochi_to_json(true) -> "true";`)
		c.writeln(`mochi_to_json(false) -> "false";`)
		c.writeln(`mochi_to_json(V) when is_integer(V); is_float(V) -> lists:flatten(io_lib:format("~p", [V]));`)
		c.writeln(`mochi_to_json(V) when is_binary(V) -> "\"" ++ mochi_escape_json(binary_to_list(V)) ++ "\"";`)
		c.writeln(`mochi_to_json(V) when is_list(V), (V =:= [] orelse is_integer(hd(V))) -> "\"" ++ mochi_escape_json(V) ++ "\"";`)
		c.writeln(`mochi_to_json(V) when is_list(V) -> "[" ++ lists:join(",", [mochi_to_json(X) || X <- V]) ++ "]";`)
		c.writeln(`mochi_to_json(V) when is_map(V) -> "{" ++ lists:join(",", ["\"" ++ atom_to_list(K) ++ "\":" ++ mochi_to_json(Val) || {K,Val} <- maps:to_list(V)]) ++ "}".`)

		c.writeln("")
		c.writeln("mochi_json(V) -> io:format(\"~s~n\", [mochi_to_json(V)]).")
	}

	if len(c.castStructs) > 0 {
		c.writeln("")
		c.writeln("mochi_to_map(V) when is_map(V) -> V;")
		c.writeln("mochi_to_map(V) when is_binary(V) -> mochi_decode_json(binary_to_list(V));")
		c.writeln("mochi_to_map(V) when is_list(V) -> mochi_decode_json(V);")
		c.writeln("mochi_to_map(_) -> #{}.")
		for _, st := range c.castStructs {
			c.writeln("")
			c.writeln(fmt.Sprintf("mochi_cast_%s(V) ->", sanitizeName(st.Name)))
			c.indent++
			c.writeln("M = mochi_to_map(V),")
			fields := []string{}
			for _, f := range st.Order {
				atom := sanitizeName(f)
				fields = append(fields, fmt.Sprintf("%s=maps:get(\"%s\", M, maps:get('%s', M, undefined))", atom, f, atom))
			}
			c.writeln(fmt.Sprintf("#%s{%s}.", sanitizeName(st.Name), strings.Join(fields, ", ")))
			c.indent--
		}
	}

	if c.needExpect {
		c.writeln("")
		c.writeln("mochi_expect(true) -> ok;")
		c.writeln("mochi_expect(_) -> erlang:error(expect_failed).")
	}

	if c.needTest {
		c.writeln("")
		c.writeln("mochi_test_start(Name) -> io:format(\"   test ~s ...\", [Name]).")
		c.writeln("mochi_test_pass(Dur) -> io:format(\" ok (~p)~n\", [Dur]).")
		c.writeln("mochi_test_fail(Err, Dur) -> io:format(\" fail ~p (~p)~n\", [Err, Dur]).")

		c.writeln("")
		c.writeln("mochi_run_test(Name, Fun) ->")
		c.indent++
		c.writeln("mochi_test_start(Name),")
		c.writeln("Start = erlang:monotonic_time(millisecond),")
		c.writeln("try Fun() of _ ->")
		c.indent++
		c.writeln("Duration = erlang:monotonic_time(millisecond) - Start,")
		c.writeln("mochi_test_pass(Duration)")
		c.indent--
		c.writeln("catch C:R ->")
		c.indent++
		c.writeln("Duration = erlang:monotonic_time(millisecond) - Start,")
		c.writeln("mochi_test_fail({C,R}, Duration)")
		c.indent--
		c.writeln("end.")
		c.indent--
	}
}
