{$mode objfpc}
program Main;
type Anon38 = record
  id: integer;
  name: string;
end;
type Anon39 = record
  id: integer;
  customerId: integer;
  total: integer;
end;
type Anon40 = record
  orderId: integer;
  customer: Anon38;
  total: integer;
end;
var
  customers: array of Anon38;
  orders: array of Anon39;
  matched2: boolean;
  result: array of Anon40;
  o: Anon39;
  c: Anon38;
  entry: integer;
begin
  customers := [(id: 1; name: 'Alice'), (id: 2; name: 'Bob')];
  orders := [(id: 100; customerId: 1; total: 250), (id: 101; customerId: 3; total: 80)];
  result := [];
  for o in orders do begin
  matched2 := false;
  for c in customers do begin
  if o.customerId = c.id then begin
  result := concat(result, [(orderId: o.id; customer: (id: 0; name: ''); total: o.total)]);
  matched2 := true;
end;
end;
  if not matched2 then begin
  result := concat(result, [(orderId: o.id; customer: (id: 0; name: ''); total: o.total)]);
end;
end;
  writeln('--- Left Join ---');
  for entry in result do begin
  writeln('Order', entry.orderId, 'customer', entry.customer, 'total', entry.total);
end;
end.
