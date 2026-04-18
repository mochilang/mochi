#!/usr/bin/env escript

main(_) ->
    Data = case io:get_chars(standard_io, "", 1000000) of
        eof -> "";
        D -> D
    end,
    Tokens = string:tokens(Data, " \n\r\t"),
    case Tokens of
        [] -> ok;
        [TStr | Rem] ->
            T = list_to_integer(TStr),
            solve(T, Rem)
    end.

solve(0, _) -> ok;
solve(T, [NStr | Rem]) ->
    N = list_to_integer(NStr),
    {NumsStr, Rem2} = lists:split(N, Rem),
    Nums = [list_to_integer(X) || X <- NumsStr],
    Ans = remove_duplicates(Nums),
    io:format("~s~n", [string:join([integer_to_list(X) || X <- Ans], " ")]),
    solve(T - 1, Rem2).

remove_duplicates([]) -> [];
remove_duplicates([H|T]) ->
    [H | remove_duplicates(H, T)].

remove_duplicates(_, []) -> [];
remove_duplicates(Prev, [H|T]) when H =:= Prev ->
    remove_duplicates(Prev, T);
remove_duplicates(_, [H|T]) ->
    [H | remove_duplicates(H, T)].
