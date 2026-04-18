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
run(T, [S1, S2, S3 | Rest], Out) ->
    Ans = case solve(S1, S2, S3) of true -> "true"; false -> "false" end,
    run(T - 1, Rest, [Ans | Out]).

solve(S1, S2, S3) ->
    M = length(S1), N = length(S2),
    if M + N =/= length(S3) -> false; true -> cell(M, N, S1, S2, S3, #{ {0,0} => true }) end.

cell(M, N, S1, S2, S3, Map0) ->
    Map1 = lists:foldl(fun(I, Acc1) ->
        lists:foldl(fun(J, Acc2) ->
            Keep = case I > 0 of true -> maps:get({I-1,J}, Acc2, false) andalso lists:nth(I, S1) =:= lists:nth(I+J, S3); false -> false end,
            Take = case J > 0 of true -> maps:get({I,J-1}, Acc2, false) andalso lists:nth(J, S2) =:= lists:nth(I+J, S3); false -> false end,
            case (I =:= 0 andalso J =:= 0) orelse Keep orelse Take of true -> maps:put({I,J}, true, Acc2); false -> Acc2 end
        end, Acc1, lists:seq(0, N))
    end, Map0, lists:seq(0, M)),
    maps:get({M,N}, Map1, false).
