#!/usr/bin/env escript
main(_) -> {ok,Bin}=file:read_file("/dev/stdin"), Lines=[string:trim(L)||L<-string:split(binary_to_list(Bin),"\n",all),L=/=[]], case Lines of []->ok; [TStr|Rest] -> run(list_to_integer(TStr), Rest, []) end.
run(0,_,Out)-> io:put_chars(string:join(lists:reverse(Out),"\n"));
run(T,Rest,Out)-> {Board,Rem}=lists:split(9,Rest), Solved=solve([list_to_tuple(R) || R <- Board]), run(T-1,Rem, lists:reverse([tuple_to_list(R) || R <- Solved]) ++ Out).
valid(B,R,C,Ch)->
    lists:all(
        fun(I) ->
            element(I+1, lists:nth(R+1,B)) =/= Ch andalso
            element(C+1, lists:nth(I+1,B)) =/= Ch
        end,
        lists:seq(0,8)
    ) andalso
    lists:all(
        fun(I) ->
            lists:all(
                fun(J) -> element(J+1, lists:nth(I+1,B)) =/= Ch end,
                lists:seq((C div 3)*3, (C div 3)*3+2)
            )
        end,
        lists:seq((R div 3)*3, (R div 3)*3+2)
    ).
set_cell(B,R,C,Ch)-> lists:sublist(B,R) ++ [setelement(C+1, lists:nth(R+1,B), Ch)] ++ lists:nthtail(R+1,B).
solve(B)-> case find_empty(B,0,0) of none -> B; {R,C} -> try_digits(B,R,C,$1) end.
find_empty(_,9,_)-> none; find_empty(B,R,9)-> find_empty(B,R+1,0); find_empty(B,R,C)-> case element(C+1, lists:nth(R+1,B)) of $. -> {R,C}; _ -> find_empty(B,R,C+1) end.
try_digits(_,_,_,Ch) when Ch > $9 -> fail;
try_digits(B,R,C,Ch)-> case valid(B,R,C,Ch) of true -> NB=set_cell(B,R,C,Ch), case solve(NB) of fail -> try_digits(B,R,C,Ch+1); Ans -> Ans end; false -> try_digits(B,R,C,Ch+1) end.
