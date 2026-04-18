#!/usr/bin/env escript
main(_) -> {ok, Bin} = file:read_file("/dev/stdin"), Lines = [string:trim(L, trailing) || L <- string:split(binary_to_list(Bin), "\n", all), L =/= ""], case Lines of [] -> ok; [TStr | Rest] -> {Out, _} = solve(list_to_integer(TStr), Rest, []), io:put_chars(string:join(Out, "\n")) end.
solve(0, Rest, Out) -> {Out, Rest};
solve(T, [NStr | Rest], Out) -> N = list_to_integer(NStr), {Words, [WStr | Tail]} = lists:split(N, Rest), Width = list_to_integer(WStr), Ans = justify(Words, Width), Block = [integer_to_list(length(Ans))] ++ ["|" ++ S ++ "|" || S <- Ans] ++ case T > 1 of true -> ["="]; false -> [] end, solve(T - 1, Tail, Out ++ Block).
justify(Words, Width) -> justify_loop(Words, Width, []).
justify_loop([], _, Acc) -> lists:reverse(Acc);
justify_loop(Words, Width, Acc) -> {LineWords, Rest, Total} = take_line(Words, Width, [], 0), Gaps = length(LineWords) - 1, Line = case Rest =:= [] orelse Gaps =:= 0 of true -> S = string:join(LineWords, " "), S ++ lists:duplicate(Width - length(S), $ ); false -> Spaces = Width - Total, Base = Spaces div Gaps, Extra = Spaces rem Gaps, build_full(LineWords, Base, Extra, []) end, justify_loop(Rest, Width, [Line | Acc]).
take_line([], _, Acc, Total) -> {lists:reverse(Acc), [], Total};
take_line([W | Rest] = Words, Width, Acc, Total) -> case Total + length(W) + length(Acc) =< Width of true -> take_line(Rest, Width, [W | Acc], Total + length(W)); false -> {lists:reverse(Acc), Words, Total} end.
build_full([W], _, _, Acc) -> lists:flatten(lists:reverse([W | Acc]));
build_full([W | Rest], Base, Extra, Acc) -> build_full(Rest, Base, erlang:max(0, Extra - 1), [lists:duplicate(Base + case Extra > 0 of true -> 1; false -> 0 end, $ ), W | Acc]).
