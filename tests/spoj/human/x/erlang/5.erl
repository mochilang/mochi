#!/usr/bin/env escript
%% https://www.spoj.com/problems/PALIN/
%% Reads a number K and prints the smallest palindrome larger than K.

main(_) ->
    {ok, [T]} = io:fread("", "~d"),
    lists:foreach(fun(_) ->
        Line = string:trim(io:get_line("")),
        io:format("~s~n", [next_pal(Line)])
    end, lists:seq(1, T)).

next_pal(Str) ->
    Digits = [C - $0 || C <- Str],
    case all_nines(Digits) of
        true -> digits_to_string([1] ++ lists:duplicate(length(Digits) - 1, 0) ++ [1]);
        false ->
            Arr = array:from_list(Digits),
            N = array:size(Arr),
            Arr1 = mirror_left_to_right(Arr, N),
            case left_smaller(Arr, N) of
                true -> digits_to_string(array:to_list(increment_middle(Arr1, N)));
                false -> digits_to_string(array:to_list(Arr1))
            end
    end.

all_nines(Ds) -> lists:all(fun(D) -> D =:= 9 end, Ds).

digits_to_string(Ds) -> [D + $0 || D <- Ds].

left_smaller(Arr, N) ->
    I = N div 2 - 1,
    J = (N + 1) div 2,
    left_smaller(Arr, I, J).

left_smaller(Arr, I, J) when I >= 0 ->
    case array:get(I, Arr) =:= array:get(J, Arr) of
        true -> left_smaller(Arr, I - 1, J + 1);
        false -> array:get(I, Arr) < array:get(J, Arr)
    end;
left_smaller(_, I, _) when I < 0 -> true.

mirror_left_to_right(Arr, N) ->
    mirror(Arr, N div 2 - 1, (N + 1) div 2).

mirror(Arr, I, J) when I >= 0 ->
    Arr1 = array:set(J, array:get(I, Arr), Arr),
    mirror(Arr1, I - 1, J + 1);
mirror(Arr, _, _) -> Arr.

increment_middle(Arr, N) ->
    I = N div 2 - 1,
    J = (N + 1) div 2,
    case N rem 2 of
        1 ->
            Mid = N div 2,
            Sum = array:get(Mid, Arr) + 1,
            Arr1 = array:set(Mid, Sum rem 10, Arr),
            inc_loop(Arr1, I, J, Sum div 10);
        0 ->
            inc_loop(Arr, I, J, 1)
    end.

inc_loop(Arr, I, J, Carry) when Carry > 0, I >= 0 ->
    Sum = array:get(I, Arr) + Carry,
    Digit = Sum rem 10,
    Carry1 = Sum div 10,
    Arr1 = array:set(I, Digit, Arr),
    Arr2 = array:set(J, Digit, Arr1),
    inc_loop(Arr2, I - 1, J + 1, Carry1);
inc_loop(Arr, _, _, _) -> Arr.
