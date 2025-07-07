:- initialization(main).
customers([
  customer{id:1,name:'Alice'},
  customer{id:2,name:'Bob'},
  customer{id:3,name:'Charlie'},
  customer{id:4,name:'Diana'}
]).
orders([
  order{id:100,customerId:1,total:250},
  order{id:101,customerId:2,total:125},
  order{id:102,customerId:1,total:300}
]).
find_orders(CId, Os, Matching) :- findall(O, (member(O,Os), O.customerId=CId), Matching).
right_join([], _, []).
right_join([C|Cs], Os, [row{customerName:C.name, order:O}|Rs]) :-
    find_orders(C.id, Os, Orders),
    (Orders=[O|_] -> true ; O=nil),
    right_join(Cs, Os, RsOrders), append([row{customerName:C.name, order:O}], RsOrders, Rs).
main :-
    customers(Cs), orders(Os),
    findall(R, (member(C,Cs), find_orders(C.id,Os,Orders), (Orders=[] -> O=nil ; member(O,Orders)), R=row{customerName:C.name, order:O}), Rows),
    writeln('--- Right Join using syntax ---'),
    forall(member(R,Rows), (
        (R.order \= nil -> format('Customer ~w has order ~w - $ ~w~n',[R.customerName,R.order.id,R.order.total]) ; format('Customer ~w has no orders~n',[R.customerName])))),
    halt.
