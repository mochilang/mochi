#!/usr/bin/env escript

main(_) ->
    {ok, Bin} = file:read_file("/dev/stdin"),
    Lines = [string:trim(L) || L <- string:split(binary_to_list(Bin), "\n", all), L =/= ""],
    case Lines of
        [] -> ok;
        [TStr | Rest] ->
            {Out, _} = run(list_to_integer(TStr), Rest, []),
            io:put_chars(string:join(Out, "\n"))
    end.

run(0, Rest, Out) -> {Out, Rest};
run(T, [NStr | Rest], Out) ->
    N = list_to_integer(NStr),
    Sols = solve(N),
    Block = [integer_to_list(length(Sols))] ++ flatten_solutions(Sols) ++ case T > 1 of true -> ["="]; false -> [] end,
    run(T - 1, Rest, Out ++ Block).

flatten_solutions([]) -> [];
flatten_solutions([Sol]) -> Sol;
flatten_solutions([Sol | Rest]) -> Sol ++ ["-"] ++ flatten_solutions(Rest).

solve(N) -> dfs(0, N, #{}, #{}, #{}, []).

dfs(R, N, _, _, _, Board) when R =:= N ->
    [lists:reverse(Board)];
dfs(R, N, Cols, D1, D2, Board) ->
    lists:append([
        begin
            A = R + C,
            B = R - C + N - 1,
            case maps:is_key(C, Cols) orelse maps:is_key(A, D1) orelse maps:is_key(B, D2) of
                true -> [];
                false ->
                    Row = lists:duplicate(C, $.) ++ "Q" ++ lists:duplicate(N - C - 1, $.),
                    dfs(R + 1, N, maps:put(C, true, Cols), maps:put(A, true, D1), maps:put(B, true, D2), [Row | Board])
            end
        end
     || C <- lists:seq(0, N - 1)]).
