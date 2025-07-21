{$mode objfpc}
program Main;
type Anon56 = record
  id: integer;
  name: string;
end;
type Anon57 = record
  id: integer;
  customerId: integer;
  total: integer;
end;
type Anon58 = record
  customerName: string;
  order: Anon57;
end;
var
  customers: array of Anon56;
  orders: array of Anon57;
  result: array of Anon58;
  c: Anon56;
  o: Anon57;
  entry: integer;
begin
  customers := [(id: 1; name: 'Alice'), (id: 2; name: 'Bob'), (id: 3; name: 'Charlie'), (id: 4; name: 'Diana')];
  orders := [(id: 100; customerId: 1; total: 250), (id: 101; customerId: 2; total: 125), (id: 102; customerId: 1; total: 300)];
  result := [];
  for c in customers do begin
  for o in orders do begin
  if o.customerId = c.id then begin
  result := concat(result, [(customerName: c.name; order: o)]);
end;
end;
end;
  writeln('--- Right Join using syntax ---');
  for entry in result do begin
  if entry.order then begin
  writeln('Customer', entry.customerName, 'has order', entry.order.id, '- $', entry.order.total);
end else begin
  writeln('Customer', entry.customerName, 'has no orders');
end;
end;
end.
