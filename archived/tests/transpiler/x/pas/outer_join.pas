{$mode objfpc}
program Main;
type Anon53 = record
  id: integer;
  name: string;
end;
type Anon54 = record
  id: integer;
  customerId: integer;
  total: integer;
end;
type Anon55 = record
  order: Anon54;
  customer: Anon53;
end;
var
  customers: array of Anon53;
  orders: array of Anon54;
  result: array of Anon55;
  o: Anon54;
  c: Anon53;
  row: integer;
begin
  customers := [(id: 1; name: 'Alice'), (id: 2; name: 'Bob'), (id: 3; name: 'Charlie'), (id: 4; name: 'Diana')];
  orders := [(id: 100; customerId: 1; total: 250), (id: 101; customerId: 2; total: 125), (id: 102; customerId: 1; total: 300), (id: 103; customerId: 5; total: 80)];
  result := [];
  for o in orders do begin
  for c in customers do begin
  if o.customerId = c.id then begin
  result := concat(result, [(order: o; customer: c)]);
end;
end;
end;
  writeln('--- Outer Join using syntax ---');
  for row in result do begin
  if row.order then begin
  if row.customer then begin
  writeln('Order', ' ', row.order.id, ' ', 'by', ' ', row.customer.name, ' ', '- $', ' ', row.order.total);
end else begin
  writeln('Order', ' ', row.order.id, ' ', 'by', ' ', 'Unknown', ' ', '- $', ' ', row.order.total);
end;
end else begin
  writeln('Customer', ' ', row.customer.name, ' ', 'has no orders');
end;
end;
end.
