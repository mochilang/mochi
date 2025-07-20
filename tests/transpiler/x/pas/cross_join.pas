{$mode objfpc}
program Main;
type Anon1 = record
  id: integer;
  name: string;
end;
type Anon2 = record
  id: integer;
  customerId: integer;
  total: integer;
end;
type Anon3 = record
  orderId: integer;
  orderCustomerId: integer;
  pairedCustomerName: string;
  orderTotal: integer;
end;
var
  customers: array of Anon1;
  orders: array of Anon2;
  result: array of Anon3;
  c: Anon1;
  entry: integer;
  o: Anon2;
begin
  customers := [(id: 1; name: 'Alice'), (id: 2; name: 'Bob'), (id: 3; name: 'Charlie')];
  orders := [(id: 100; customerId: 1; total: 250), (id: 101; customerId: 2; total: 125), (id: 102; customerId: 1; total: 300)];
  result := [];
  for o in orders do begin
  for c in customers do begin
  result := concat(result, [(orderId: o.id; orderCustomerId: o.customerId; pairedCustomerName: c.name; orderTotal: o.total)]);
end;
end;
  writeln('--- Cross Join: All order-customer pairs ---');
  for entry in result do begin
  writeln('Order', entry.orderId, '(customerId:', entry.orderCustomerId, ', total: $', entry.orderTotal, ') paired with', entry.pairedCustomerName);
end;
end.
