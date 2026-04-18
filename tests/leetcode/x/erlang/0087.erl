#!/usr/bin/env escript
main(_) ->
    {ok, Input} = file:read_file("/dev/stdin"),
    Lines = string:split(binary_to_list(Input), "\n", all),
    case Lines of
        [] -> ok;
        [T0 | _] when T0 =:= "" -> ok;
        [T0 | Rest] ->
            T = list_to_integer(string:trim(T0)),
            io:put_chars(run(T, Rest, []))
    end.

run(0, _Rest, Out) -> string:join(lists:reverse(Out), "\n");
run(T, [S1, S2 | Rest], Out) ->
    M = maps:new(),
    Ans = case dfs(string:trim(S1), string:trim(S2), 1, 1, length(string:trim(S1)), M) of {true, _} -> "true"; _ -> "false" end,
    run(T - 1, Rest, [Ans | Out]).

dfs(S1, S2, I1, I2, Len, Memo) ->
    Key = {I1, I2, Len},
    case maps:find(Key, Memo) of
        {ok, V} -> {V, Memo};
        error ->
            A = string:substr(S1, I1, Len),
            B = string:substr(S2, I2, Len),
            case A =:= B of
                true -> {true, maps:put(Key, true, Memo)};
                false ->
                    case lists:sort(A) =:= lists:sort(B) of
                        false -> {false, maps:put(Key, false, Memo)};
                        true -> split_try(S1, S2, I1, I2, Len, 1, Memo, Key)
                    end
            end
    end.

split_try(_S1, _S2, _I1, _I2, Len, K, Memo, Key) when K >= Len -> {false, maps:put(Key, false, Memo)};
split_try(S1, S2, I1, I2, Len, K, Memo, Key) ->
    {A1, Memo1} = dfs(S1, S2, I1, I2, K, Memo),
    {A2, Memo2} = dfs(S1, S2, I1 + K, I2 + K, Len - K, Memo1),
    case A1 andalso A2 of
        true -> {true, maps:put(Key, true, Memo2)};
        false ->
            {B1, Memo3} = dfs(S1, S2, I1, I2 + Len - K, K, Memo2),
            {B2, Memo4} = dfs(S1, S2, I1 + K, I2, Len - K, Memo3),
            case B1 andalso B2 of
                true -> {true, maps:put(Key, true, Memo4)};
                false -> split_try(S1, S2, I1, I2, Len, K + 1, Memo4, Key)
            end
    end.
