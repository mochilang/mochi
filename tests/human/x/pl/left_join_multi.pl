:- initialization(main).
customers([
  customer{id:1,name:'Alice'},
  customer{id:2,name:'Bob'}
]).
orders([
  order{id:100,customerId:1},
  order{id:101,customerId:2}
]).
items([
  item{orderId:100, sku:'a'}
]).
find_customer(Id, Cs, C) :- (member(X,Cs), X.id=Id -> C=X ; C=nil).
find_item(OrderId, Is, I) :- (member(X,Is), X.orderId=OrderId -> I=X ; I=nil).
left_join_multi([], _, _, []).
left_join_multi([O|Os], Cs, Is, [res{orderId:O.id,name:Name,item:I}|Rs]) :-
    find_customer(O.customerId,Cs,C),
    Name=C.name,
    find_item(O.id,Is,I),
    left_join_multi(Os, Cs, Is, Rs).
format_item(nil,'<nil>').
format_item(I,S) :- I \= nil, format(atom(S),'map[orderId:~w sku:~w]',[I.orderId,I.sku]).
main :-
    customers(Cs), orders(Os), items(Is),
    left_join_multi(Os, Cs, Is, Res),
    writeln('--- Left Join Multi ---'),
    forall(member(R,Res),(format_item(R.item,S),format('~w ~w ~w~n',[R.orderId,R.name,S]))),
    halt.
