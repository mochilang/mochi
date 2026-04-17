#!/usr/bin/env escript

my_atoi(S) ->
    Trimmed = drop_spaces(S),
    {Sign, Rest} = case Trimmed of
        [$- | T] -> {-1, T};
        [$+ | T] -> {1, T};
        _ -> {1, Trimmed}
    end,
    Limit = case Sign of 1 -> 7; _ -> 8 end,
    parse_digits(Rest, Sign, Limit, 0).

parse_digits([Ch | Rest], Sign, Limit, Ans) when Ch >= $0, Ch =< $9 ->
    Digit = Ch - $0,
    case (Ans > 214748364) orelse (Ans =:= 214748364 andalso Digit > Limit) of
        true -> case Sign of 1 -> 2147483647; _ -> -2147483648 end;
        false -> parse_digits(Rest, Sign, Limit, Ans * 10 + Digit)
    end;
parse_digits(_, Sign, _, Ans) -> Sign * Ans.

drop_spaces([$  | T]) -> drop_spaces(T);
drop_spaces(S) -> S.
trim_cr(Line) -> lists:reverse(drop_cr(lists:reverse(Line))).
drop_cr([$\r | T]) -> T;
drop_cr(L) -> L.

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Lines = [trim_cr(L) || L <- string:split(binary_to_list(Data), "\n", all)],
    case Lines of
        [] -> ok;
        [First | Rest] ->
            T = list_to_integer(string:trim(First)),
            Padded = Rest ++ lists:duplicate(T, ""),
            Outs = [integer_to_list(my_atoi(lists:nth(I, Padded))) || I <- lists:seq(1, T)],
            io:format("~s", [string:join(Outs, "\n")])
    end.
