#!/usr/bin/env escript
main(_) ->
  {ok, Bin} = file:read_file("/dev/stdin"),
  Lines = [string:trim(L) || L <- string:split(binary_to_list(Bin), "\n", all), L =/= []],
  case Lines of [] -> ok; [TStr|Rest] -> run(list_to_integer(TStr), Rest, []) end.

run(0, _, Out) ->
  io:put_chars(string:join(lists:reverse(Out), "\n"));
run(T, [NStr|Rest], Out) ->
  N = list_to_integer(NStr),
  {Take, [KStr|Rem]} = lists:split(N, Rest),
  K = list_to_integer(KStr),
  Arr = [list_to_integer(X) || X <- Take],
  run(T - 1, Rem, [fmt(reverse_groups(Arr, K)) | Out]).

reverse_groups(Arr, K) when length(Arr) < K ->
  Arr;
reverse_groups(Arr, K) ->
  {Chunk, Rest} = lists:split(K, Arr),
  lists:reverse(Chunk) ++ reverse_groups(Rest, K).

fmt(Arr) ->
  "[" ++ string:join([integer_to_list(X) || X <- Arr], ",") ++ "]".
