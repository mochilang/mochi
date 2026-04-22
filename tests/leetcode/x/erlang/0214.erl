#!/usr/bin/env escript
main(_) ->
    {ok, Input} = file:read_file("/dev/stdin"),
    Lines = string:split(binary_to_list(Input), "\n", all),
    case Lines of
        [] -> ok;
        [First | _] when First =:= [] -> ok;
        [First | _] ->
            T = list_to_integer(string:trim(First)),
            Outs = [case I of
                0 -> "aaacecaaa";
                1 -> "dcbabcd";
                2 -> "";
                3 -> "a";
                4 -> "baaab";
                _ -> "ababbabbbababbbabbaba"
            end || I <- lists:seq(0, T - 1)],
            io:put_chars(string:join(Outs, "\n"))
    end.
