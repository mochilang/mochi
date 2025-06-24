package erlcode

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
		c.writeln("mochi_get(M, K) when is_list(M), is_integer(K) -> lists:nth(K + 1, M);")
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
		c.writeln("mochi_load(Path, _Opts) ->")
		c.indent++
		c.writeln("case file:read_file(Path) of")
		c.indent++
		c.writeln("{ok, Bin} ->")
		c.indent++
		c.writeln("Ext = filename:extension(Path),")
		c.writeln("case Ext of")
		c.indent++
		c.writeln("\".txt\" -> binary_to_list(Bin);")
		c.writeln("_ -> binary_to_term(Bin)")
		c.indent--
		c.writeln("end;")
		c.writeln("_ -> []")
		c.indent--
		c.writeln("end.")
		c.indent--

		c.writeln("")
		c.writeln("mochi_save(Data, Path, _Opts) ->")
		c.indent++
		c.writeln("Ext = filename:extension(Path),")
		c.writeln("Bin = case Ext of")
		c.indent++
		c.writeln("\".txt\" -> Data;")
		c.writeln("_ -> term_to_binary(Data)")
		c.indent--
		c.writeln("end,")
		c.writeln("ok = file:write_file(Path, Bin).")
		c.indent--
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
		c.writeln("case httpc:request(Method, {Url, []}, [], []) of")
		c.indent++
		c.writeln("{ok, {{_, 200, _}, _H, Body}} -> Body;")
		c.writeln("_ -> []")
		c.indent--
		c.writeln("end.")
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
