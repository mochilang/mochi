{$mode objfpc}
program Main;
type Anon33 = record
  id: integer;
  name: string;
end;
type Anon34 = record
  id: integer;
  customerId: integer;
end;
type Anon35 = record
  orderId: integer;
  sku: string;
end;
type Anon36 = record
  name: string;
  sku: string;
end;
var
  customers: array of Anon33;
  orders: array of Anon34;
  items: array of Anon35;
  result: array of Anon36;
  r: integer;
  o: Anon34;
  c: Anon33;
  i: Anon35;
begin
  customers := [(id: 1; name: 'Alice'), (id: 2; name: 'Bob')];
  orders := [(id: 100; customerId: 1), (id: 101; customerId: 2)];
  items := [(orderId: 100; sku: 'a'), (orderId: 101; sku: 'b')];
  result := [];
  for o in orders do begin
  for c in customers do begin
  for i in items do begin
  if (o.customerId = c.id) and (o.id = i.orderId) then begin
  result := concat(result, [(name: c.name; sku: i.sku)]);
end;
end;
end;
end;
  writeln('--- Multi Join ---');
  for r in result do begin
  writeln(r.name, 'bought item', r.sku);
end;
end.
