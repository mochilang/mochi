#!/usr/bin/env escript

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Tokens = string:tokens(binary_to_list(Data), " \n\r\t"),
    case Tokens of
        [] -> ok;
        [TStr | Rest] ->
            T = list_to_integer(TStr),
            lists:foreach(
                fun(S) -> io:format("~s~n", [case is_valid(S, []) of true -> "true"; false -> "false" end]) end,
                lists:sublist(Rest, T)
            )
    end.

is_valid([], Stack) -> Stack =:= [];
is_valid([C | Rest], Stack) when C =:= $(; C =:= $[; C =:= ${ ->
    is_valid(Rest, [C | Stack]);
is_valid([C | Rest], [Open | Stack]) ->
    case match(C, Open) of
        true -> is_valid(Rest, Stack);
        false -> false
    end;
is_valid([_ | _], []) -> false.

match($), $() -> true;
match($], $[) -> true;
match($}, ${) -> true;
match(_, _) -> false.
