{$mode objfpc}
program Main;
type Anon30 = record
  id: integer;
  name: string;
end;
type Anon31 = record
  id: integer;
  customerId: integer;
  total: integer;
end;
type Anon32 = record
  orderId: integer;
  customerName: string;
  total: integer;
end;
var
  customers: array of Anon30;
  orders: array of Anon31;
  result: array of Anon32;
  o: Anon31;
  c: Anon30;
  entry: integer;
begin
  customers := [(id: 1; name: 'Alice'), (id: 2; name: 'Bob'), (id: 3; name: 'Charlie')];
  orders := [(id: 100; customerId: 1; total: 250), (id: 101; customerId: 2; total: 125), (id: 102; customerId: 1; total: 300), (id: 103; customerId: 4; total: 80)];
  result := [];
  for o in orders do begin
  for c in customers do begin
  if o.customerId = c.id then begin
  result := concat(result, [(orderId: o.id; customerName: c.name; total: o.total)]);
end;
end;
end;
  writeln('--- Orders with customer info ---');
  for entry in result do begin
  writeln('Order', entry.orderId, 'by', entry.customerName, '- $', entry.total);
end;
end.
