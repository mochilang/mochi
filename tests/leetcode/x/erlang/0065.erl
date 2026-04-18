#!/usr/bin/env escript

main(_) ->
    {ok, Bin} = file:read_file("/dev/stdin"),
    Lines = [string:trim(L, trailing) || L <- string:split(binary_to_list(Bin), "\n", all)],
    case Lines of
        [] -> ok;
        [TStr | Rest] ->
            T = list_to_integer(string:trim(TStr)),
            Out = [case valid_number(lists:nth(I, Rest)) of true -> "true"; false -> "false" end || I <- lists:seq(1, T)],
            io:put_chars(string:join(Out, "\n"))
    end.

valid_number(S) -> scan(S, 1, false, false, false, true, 0).

scan([], _, SeenDigit, _, _, DigitAfterExp, _) -> SeenDigit andalso DigitAfterExp;
scan([Ch | Rest], Pos, SeenDigit, SeenDot, SeenExp, DigitAfterExp, Prev) ->
    case Ch of
        _ when Ch >= $0, Ch =< $9 ->
            scan(Rest, Pos + 1, true, SeenDot, SeenExp, case SeenExp of true -> true; false -> DigitAfterExp end, Ch);
        $+ ->
            case Pos =:= 1 orelse Prev =:= $e orelse Prev =:= $E of
                true -> scan(Rest, Pos + 1, SeenDigit, SeenDot, SeenExp, DigitAfterExp, Ch);
                false -> false
            end;
        $- ->
            case Pos =:= 1 orelse Prev =:= $e orelse Prev =:= $E of
                true -> scan(Rest, Pos + 1, SeenDigit, SeenDot, SeenExp, DigitAfterExp, Ch);
                false -> false
            end;
        $. ->
            case SeenDot orelse SeenExp of
                true -> false;
                false -> scan(Rest, Pos + 1, SeenDigit, true, SeenExp, DigitAfterExp, Ch)
            end;
        $e ->
            case SeenExp orelse not SeenDigit of
                true -> false;
                false -> scan(Rest, Pos + 1, SeenDigit, SeenDot, true, false, Ch)
            end;
        $E ->
            case SeenExp orelse not SeenDigit of
                true -> false;
                false -> scan(Rest, Pos + 1, SeenDigit, SeenDot, true, false, Ch)
            end;
        _ -> false
    end.
