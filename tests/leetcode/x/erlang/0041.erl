#!/usr/bin/env escript
main(_) ->
    {ok, Bin} = file:read_file("/dev/stdin"),
    Lines = [string:trim(L) || L <- string:split(binary_to_list(Bin), "
", all), L =/= ""],
    case Lines of
        [] -> ok;
        [TStr | Rest] -> run(list_to_integer(TStr), Rest, [])
    end.

run(0, _, Out) -> io:put_chars(string:join(lists:reverse(Out), "
"));
run(T, [NStr | Rest], Out) ->
    N = list_to_integer(NStr),
    {Vals, Rem} = lists:split(N, Rest),
    Nums = [list_to_integer(V) || V <- Vals],
    run(T - 1, Rem, [integer_to_list(first_missing_positive(Nums)) | Out]).

first_missing_positive(Nums) ->
    Arr = array:from_list(Nums),
    N = array:size(Arr),
    Arr2 = place(Arr, N, 0),
    scan(Arr2, N, 0).

place(Arr, N, I) when I >= N -> Arr;
place(Arr, N, I) ->
    V = array:get(I, Arr),
    case V >= 1 andalso V =< N andalso array:get(V - 1, Arr) =/= V of
        true ->
            Target = array:get(V - 1, Arr),
            Arr2 = array:set(V - 1, V, array:set(I, Target, Arr)),
            place(Arr2, N, I);
        false ->
            place(Arr, N, I + 1)
    end.

scan(_Arr, N, I) when I >= N -> N + 1;
scan(Arr, N, I) ->
    case array:get(I, Arr) =/= I + 1 of
        true -> I + 1;
        false -> scan(Arr, N, I + 1)
    end.
