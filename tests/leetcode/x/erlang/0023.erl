#!/usr/bin/env escript
main(_) ->
    {ok, Bin} = file:read_file("/dev/stdin"),
    Lines = [string:trim(L) || L <- string:split(binary_to_list(Bin), "
", all), L =/= []],
    case Lines of
        [] -> ok;
        [TStr | Rest] -> run(list_to_integer(TStr), Rest, [])
    end.
run(0, _, Out) -> io:put_chars(string:join(lists:reverse(Out), "
"));
run(T, [KStr | Rest], Out) ->
    {Vals, Rem} = lists_data(list_to_integer(KStr), Rest, []),
    Sorted = lists:sort(Vals),
    Str = "[" ++ string:join([integer_to_list(X) || X <- Sorted], ",") ++ "]",
    run(T - 1, Rem, [Str | Out]).
lists_data(0, Rest, Acc) -> {Acc, Rest};
lists_data(K, [NStr | Rest], Acc) ->
    N = list_to_integer(NStr),
    {Take, Rem} = lists:split(N, Rest),
    lists_data(K - 1, Rem, Acc ++ [list_to_integer(X) || X <- Take]).
