#!/usr/bin/env escript
main(_) -> {ok,Bin}=file:read_file("/dev/stdin"), Lines=[string:trim(L)||L<-string:split(binary_to_list(Bin),"
",all),L=/=[]], case Lines of []->ok; [TStr|Rest] -> run(list_to_integer(TStr), Rest, []) end.
run(0,_,Out)-> io:put_chars(string:join(lists:reverse(Out),"
"));
run(T,[NStr|Rest],Out)-> N=list_to_integer(NStr), {Vals,Rem}=lists:split(N,Rest), Arr=[list_to_integer(V)||V<-Vals], run(T-1,Rem,[integer_to_list(trap(Arr))|Out]).
trap(H)-> go(H,1,length(H),0,0,0).
go(_,L,R,_,_,W) when L > R -> W;
go(H,L,R,LM,RM,W)-> if LM =< RM -> V=lists:nth(L,H), if V < LM -> go(H,L+1,R,LM,RM,W+LM-V); true -> go(H,L+1,R,V,RM,W) end; true -> V=lists:nth(R,H), if V < RM -> go(H,L,R-1,LM,RM,W+RM-V); true -> go(H,L,R-1,LM,V,W) end end.
