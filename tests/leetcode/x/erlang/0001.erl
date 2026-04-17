#!/usr/bin/env escript

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Tokens = string:tokens(binary_to_list(Data), " \n\r\t"),
    case Tokens of
        [] -> ok;
        _ ->
            Ints = [list_to_integer(T) || T <- Tokens],
            [T | Rest] = Ints,
            Lines = solve(Rest, T, []),
            io:format("~s", [string:join(Lines, "\n")])
    end.

solve(Rest, 0, Acc) ->
    lists:reverse(Acc);
solve([N, Target | Rest], T, Acc) ->
    {Nums, Tail} = lists:split(N, Rest),
    {A, B} = two_sum(Nums, Target, 0),
    solve(Tail, T - 1, [integer_to_list(A) ++ " " ++ integer_to_list(B) | Acc]).

two_sum(Nums, Target, I) when I >= length(Nums) ->
    {0, 0};
two_sum(Nums, Target, I) ->
    case two_sum_inner(Nums, Target, I, I + 1) of
        none -> two_sum(Nums, Target, I + 1);
        Ans -> Ans
    end.

two_sum_inner(Nums, _Target, _I, J) when J >= length(Nums) ->
    none;
two_sum_inner(Nums, Target, I, J) ->
    A = lists:nth(I + 1, Nums),
    B = lists:nth(J + 1, Nums),
    case A + B =:= Target of
        true -> {I, J};
        false -> two_sum_inner(Nums, Target, I, J + 1)
    end.
