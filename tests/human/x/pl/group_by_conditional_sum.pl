:- initialization(main).

items([
  item{cat:a, val:10, flag:true},
  item{cat:a, val:5, flag:false},
  item{cat:b, val:20, flag:true}
]).

sum_vals(Items, Cat, Total, TrueTotal) :-
    findall(V, (member(I, Items), I.cat=Cat, V=I.val), Vals),
    findall(V, (member(I, Items), I.cat=Cat, I.flag=true, V=I.val), TrueVals),
    sum_list(Vals, Total),
    sum_list(TrueVals, TrueTotal).

share(Items, Cat, Share) :-
    sum_vals(Items, Cat, Total, TrueTotal),
    Share is TrueTotal / Total.

main :-
    items(Items),
    share(Items, a, ShareA),
    share(Items, b, ShareB),
    format('map[cat:a share:~g] map[cat:b share:~g]~n', [ShareA, ShareB]),
    halt.
