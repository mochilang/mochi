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
run(T, [NStr, KStr | Rest], Out) ->
    N = list_to_integer(NStr),
    K = list_to_integer(KStr),
    run(T - 1, Rest, Out ++ [get_permutation(N, K)]).

fact(0) -> 1;
fact(N) -> N * fact(N - 1).

remove_at(0, [H | T]) -> {H, T};
remove_at(I, [H | T]) ->
    {X, Rest} = remove_at(I - 1, T),
    {X, [H | Rest]}.

build(_Digits, _K, 0, Acc) -> lists:flatten(lists:reverse(Acc));
build(Digits, K, Rem, Acc) ->
    Block = fact(Rem - 1),
    Idx = K div Block,
    K2 = K rem Block,
    {Pick, Rest} = remove_at(Idx, Digits),
    build(Rest, K2, Rem - 1, [Pick | Acc]).

get_permutation(N, K) ->
    Digits = [integer_to_list(I) || I <- lists:seq(1, N)],
    build(Digits, K - 1, N, []).
