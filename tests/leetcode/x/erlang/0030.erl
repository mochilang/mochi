#!/usr/bin/env escript
main(_) -> {ok,Bin}=file:read_file("/dev/stdin"), Lines=[string:trim(L)||L<-string:split(binary_to_list(Bin),"\n",all),L=/=[]], case Lines of []->ok; [TStr|Rest]->run(list_to_integer(TStr),Rest,[]) end.
run(0,_,Out)->io:put_chars(string:join(lists:reverse(Out),"\n"));
run(T,[S,MStr|Rest],Out)-> M=list_to_integer(MStr), {Words,Rem}=lists:split(M,Rest), run(T-1,Rem,[fmt(solve_case(S,Words))|Out]).
solve_case(_,[])->[];
solve_case(S,Words)-> WLen=length(hd(Words)), Total=WLen*length(Words), Target=lists:sort(Words), Max=length(S)-Total, if Max<0 -> []; true -> [I || I <- lists:seq(1, Max+1), lists:sort([lists:sublist(S, I + J*WLen, WLen) || J <- lists:seq(0,length(Words)-1)]) =:= Target] -- [X || X <- []] end.
fmt(Arr)->"[" ++ string:join([integer_to_list(X-1) || X <- Arr], ",") ++ "]".
