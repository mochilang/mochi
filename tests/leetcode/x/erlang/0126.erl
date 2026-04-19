#!/usr/bin/env escript

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Lines = [string:trim(L, trailing, "\r") || L <- string:split(binary_to_list(Data), "\n", all), L =/= []],
    case Lines of
        [] -> ok;
        [First | Rest] ->
            Tc = list_to_integer(First),
            {_, Outs} = solve_cases(Tc, Rest, []),
            io:format("~s", [string:join(lists:reverse(Outs), "\n\n")])
    end.

solve_cases(0, Rest, Acc) -> {Rest, Acc};
solve_cases(Tc, [Begin, End, NStr | Rest], Acc) ->
    N = list_to_integer(NStr),
    {Words, Tail} = lists:split(N, Rest),
    Paths = ladders(Begin, End, Words),
    solve_cases(Tc - 1, Tail, [format_paths(Paths) | Acc]).

ladders(Begin, End, Words) ->
    WordSet = sets:from_list(Words),
    case sets:is_element(End, WordSet) of
        false -> [];
        true ->
            {Parents, Found} = bfs([Begin], sets:from_list([Begin]), maps:new(), End, WordSet),
            case Found of
                false -> [];
                true -> lists:sort(build_paths(End, Begin, Parents))
            end
    end.

bfs([], _Visited, Parents, _End, _WordSet) -> {Parents, false};
bfs(Level, Visited, Parents, End, WordSet) ->
    {NextRev, Parents1, Found} =
        lists:foldl(
            fun(Word, {Next0, Parents0, Found0}) ->
                lists:foldl(
                    fun(Nw, {Next1, Parents2, Found1}) ->
                        case sets:is_element(Nw, WordSet) andalso not sets:is_element(Nw, Visited) of
                            true ->
                                Next2 = case lists:member(Nw, Next1) of true -> Next1; false -> [Nw | Next1] end,
                                Prev = maps:get(Nw, Parents2, []),
                                Parents3 = maps:put(Nw, [Word | Prev], Parents2),
                                {Next2, Parents3, Found1 orelse Nw =:= End};
                            false ->
                                {Next1, Parents2, Found1}
                        end
                    end,
                    {Next0, Parents0, Found0},
                    neighbors(Word))
            end,
            {[], Parents, false},
            lists:sort(Level)),
    case Found of
        true -> {Parents1, true};
        false ->
            Next = lists:reverse(NextRev),
            Visited1 = lists:foldl(fun sets:add_element/2, Visited, Next),
            bfs(Next, Visited1, Parents1, End, WordSet)
    end.

neighbors(Word) ->
    Chars = Word,
    lists:append([replace_at(Chars, I, C) || I <- lists:seq(1, length(Chars)), C <- lists:seq($a, $z), C =/= lists:nth(I, Chars)]).

replace_at(Chars, I, C) ->
    {Left, [_ | Right]} = lists:split(I - 1, Chars),
    [Left ++ [C] ++ Right].

build_paths(Word, Begin, _Parents) when Word =:= Begin -> [[Begin]];
build_paths(Word, Begin, Parents) ->
    Prev = lists:sort(maps:get(Word, Parents, [])),
    lists:append([
        [Path ++ [Word] || Path <- build_paths(P, Begin, Parents)]
        || P <- Prev
    ]).

format_paths(Paths) ->
    Count = integer_to_list(length(Paths)),
    Lines = [string:join(Path, "->") || Path <- Paths],
    string:join([Count | Lines], "\n").
