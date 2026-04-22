#!/usr/bin/env escript

solve(Vals, Ok) ->
    {_Gain, Best} = dfs(0, Vals, Ok, -1000000000),
    Best.

dfs(I, Vals, _Ok, Best) when I >= length(Vals) -> {0, Best};
dfs(I, Vals, Ok, Best) ->
    case lists:nth(I + 1, Ok) of
        false -> {0, Best};
        true ->
            {Left0, Best1} = dfs(2 * I + 1, Vals, Ok, Best),
            {Right0, Best2} = dfs(2 * I + 2, Vals, Ok, Best1),
            Left = erlang:max(0, Left0),
            Right = erlang:max(0, Right0),
            Val = lists:nth(I + 1, Vals),
            Best3 = erlang:max(Best2, Val + Left + Right),
            {Val + erlang:max(Left, Right), Best3}
    end.

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Lines = [string:trim(L, trailing, "\r") || L <- string:split(binary_to_list(Data), "\n", all), L =/= []],
    case Lines of
        [] -> ok;
        [First | Rest] ->
            Tc = list_to_integer(string:trim(First)),
            {_, Outs} = build(Tc, Rest, []),
            io:format("~s", [string:join(lists:reverse(Outs), "\n")])
    end.

build(0, Rest, Acc) -> {Rest, Acc};
build(Tc, [NStr | Rest], Acc) ->
    N = list_to_integer(string:trim(NStr)),
    {Toks, Tail} = lists:split(N, Rest),
    Vals = [case Tok of "null" -> 0; _ -> list_to_integer(string:trim(Tok)) end || Tok <- Toks],
    Ok = [Tok =/= "null" || Tok <- Toks],
    build(Tc - 1, Tail, [integer_to_list(solve(Vals, Ok)) | Acc]).
