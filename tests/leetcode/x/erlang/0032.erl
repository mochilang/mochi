#!/usr/bin/env escript
main(_) -> {ok,Bin}=file:read_file("/dev/stdin"), Lines=[string:trim(L) || L <- string:split(binary_to_list(Bin),"\n",all)], case Lines of []->ok; [TStr|Rest] when TStr=/=[] -> run(list_to_integer(TStr), Rest, []) ; _ -> ok end.
run(0,_,Out)->io:put_chars(string:join(lists:reverse(Out),"\n"));
run(T,[NStr|Rest],Out)-> N=list_to_integer(NStr), {S,Rem}=if N>0 -> [H|R]=Rest, {H,R}; true -> {[],Rest} end, run(T-1,Rem,[integer_to_list(solve(S))|Out]).
solve(S)-> solve(S, 0, [-1], 0).
solve([], _, _, Best)-> Best;
solve([$(|Rest], I, Stack, Best)-> solve(Rest, I+1, [I|Stack], Best);
solve([$)|Rest], I, [_|Stack], Best)-> case Stack of [] -> solve(Rest, I+1, [I], Best); [H|_] -> B = case I-H of X when X > Best -> X; _ -> Best end, solve(Rest, I+1, Stack, B) end.
