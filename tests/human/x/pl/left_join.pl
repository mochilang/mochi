:- initialization(main).

customers([
  customer{id:1, name:'Alice'},
  customer{id:2, name:'Bob'}
]).

orders([
  order{id:100, customerId:1, total:250},
  order{id:101, customerId:3, total:80}
]).

find_customer(Id, Customers, Cust) :-
    (member(C, Customers), C.id = Id -> Cust = C ; Cust = nil).

left_join([], _, []).
left_join([O|Os], Cs, [entry{orderId:O.id, customer:Cust, total:O.total}|Rs]) :-
    find_customer(O.customerId, Cs, Cust),
    left_join(Os, Cs, Rs).

format_customer(nil, '<nil>').
format_customer(C, Str) :-
    C \= nil,
    format(atom(Str), 'map[id:~w name:~w]', [C.id, C.name]).

main :-
    customers(Cs), orders(Os), left_join(Os, Cs, Result),
    writeln('--- Left Join ---'),
    forall(member(E, Result),
        (format_customer(E.customer, CS),
         format('Order ~w customer ~w total ~w~n', [E.orderId, CS, E.total]))),
    halt.
