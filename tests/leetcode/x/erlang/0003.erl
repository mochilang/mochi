#!/usr/bin/env escript
main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Lines0 = string:split(binary_to_list(Data), "
", all),
    Lines = [trim_cr(L) || L <- Lines0],
    case Lines of
        [] -> ok;
        [First | Rest] ->
            T = list_to_integer(string:trim(First)),
            Out = [integer_to_list(longest(lists:nth(I, Rest))) || I <- lists:seq(1, T)],
            io:format("~s", [string:join(Out, "
")])
    end.
trim_cr(Line) -> lists:reverse(drop_cr(lists:reverse(Line))).
drop_cr([$ | T]) -> T; drop_cr(L) -> L.
longest(S) -> longest(S, 1, 1, #{}, 0).
longest([], _, _, _, Best) -> Best;
longest([Ch | Rest], Pos, Left, Last, Best) ->
    Prev = maps:get(Ch, Last, 0),
    Left2 = if Prev >= Left -> Prev + 1; true -> Left end,
    Best2 = max(Best, Pos - Left2 + 1),
    longest(Rest, Pos + 1, Left2, maps:put(Ch, Pos, Last), Best2).
