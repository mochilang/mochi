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
  order{id:102,customerId:1,total:300},
  order{id:103,customerId:5,total:80}
]).
find_customer(Id,Cs,C) :- (member(X,Cs),X.id=Id->C=X;C=nil).
outer_join([],_,[]).
outer_join([O|Os],Cs,[row{order:O,customer:C}|Rs]) :-
    find_customer(O.customerId,Cs,C), outer_join(Os,Cs,Rs).
main :-
    customers(Cs), orders(Os), outer_join(Os,Cs,Rows),
    writeln('--- Outer Join using syntax ---'),
    forall(member(R,Rows), (
        (R.order \= nil -> (R.customer \= nil -> format('Order ~w by ~w - $ ~w~n',[R.order.id,R.customer.name,R.order.total]) ; format('Order ~w by Unknown - $ ~w~n',[R.order.id,R.order.total])) ; format('Customer ~w has no orders~n',[R.customer.name]))
    )),
    halt.
