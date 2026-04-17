#!/usr/bin/env escript
main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Tokens = string:tokens(binary_to_list(Data), " \n\r\t"),
    case Tokens of
        [] -> ok;
        [TStr | Rest] ->
            T = list_to_integer(TStr),
            {_, Lines} = solve(T, Rest, []),
            io:format("~s", [string:join(lists:reverse(Lines), "\n")])
    end.
solve(0, Rest, Acc) -> {Rest, Acc};
solve(T, [NStr | Rest], Acc) ->
    N = list_to_integer(NStr),
    {A, Rest1} = take_ints(N, Rest, []),
    [MStr | Rest2] = Rest1,
    M = list_to_integer(MStr),
    {B, Rest3} = take_ints(M, Rest2, []),
    solve(T - 1, Rest3, [fmt(add_lists(A, B, 0, [])) | Acc]).
take_ints(0, Rest, Acc) -> {lists:reverse(Acc), Rest};
take_ints(N, [X | Rest], Acc) -> take_ints(N - 1, Rest, [list_to_integer(X) | Acc]).
add_lists([], [], 0, Acc) -> lists:reverse(Acc);
add_lists([], [], Carry, Acc) -> lists:reverse([Carry | Acc]);
add_lists([X | Xs], [], Carry, Acc) -> Sum = X + Carry, add_lists(Xs, [], Sum div 10, [Sum rem 10 | Acc]);
add_lists([], [Y | Ys], Carry, Acc) -> Sum = Y + Carry, add_lists([], Ys, Sum div 10, [Sum rem 10 | Acc]);
add_lists([X | Xs], [Y | Ys], Carry, Acc) -> Sum = X + Y + Carry, add_lists(Xs, Ys, Sum div 10, [Sum rem 10 | Acc]).
fmt([]) -> "[]";
fmt([X | Xs]) -> "[" ++ integer_to_list(X) ++ lists:flatten(["," ++ integer_to_list(V) || V <- Xs]) ++ "]".
