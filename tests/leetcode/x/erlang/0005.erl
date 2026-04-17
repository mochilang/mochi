#!/usr/bin/env escript

expand(S, Left, Right) when Left >= 0, Right < length(S) ->
    case lists:nth(Left + 1, S) =:= lists:nth(Right + 1, S) of
        true -> expand(S, Left - 1, Right + 1);
        false -> {Left + 1, Right - Left - 1}
    end;
expand(_, Left, Right) ->
    {Left + 1, Right - Left - 1}.

longest_palindrome(S) ->
    N = length(S),
    Best0 = case N > 0 of true -> {0, 1}; false -> {0, 0} end,
    {BestStart, BestLen} =
        lists:foldl(fun(I, {BS, BL}) ->
            {S1, L1} = expand(S, I, I),
            {BS1, BL1} = case L1 > BL of true -> {S1, L1}; false -> {BS, BL} end,
            {S2, L2} = expand(S, I, I + 1),
            case L2 > BL1 of true -> {S2, L2}; false -> {BS1, BL1} end
        end, Best0, lists:seq(0, N - 1)),
    lists:sublist(S, BestStart + 1, BestLen).

trim_cr(Line) -> lists:reverse(drop_cr(lists:reverse(Line))).
drop_cr([$\r | T]) -> T;
drop_cr(L) -> L.

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Lines0 = string:split(binary_to_list(Data), "\n", all),
    Lines = [trim_cr(L) || L <- Lines0],
    case Lines of
        [] -> ok;
        [First | Rest] ->
            T = list_to_integer(string:trim(First)),
            Inputs = lists:sublist(Rest ++ lists:duplicate(T, ""), T),
            Outs = [longest_palindrome(S) || S <- Inputs],
            io:format("~s", [string:join(Outs, "\n")])
    end.
