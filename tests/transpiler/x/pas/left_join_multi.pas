{$mode objfpc}
program Main;
type Anon41 = record
  id: integer;
  name: string;
end;
type Anon42 = record
  id: integer;
  customerId: integer;
end;
type Anon43 = record
  orderId: integer;
  sku: string;
end;
type Anon44 = record
  orderId: integer;
  name: string;
  item: Anon43;
end;
var
  customers: array of Anon41;
  orders: array of Anon42;
  items: array of Anon43;
  result: array of Anon44;
  o: Anon42;
  c: Anon41;
  i: Anon43;
  r: integer;
begin
  customers := [(id: 1; name: 'Alice'), (id: 2; name: 'Bob')];
  orders := [(id: 100; customerId: 1), (id: 101; customerId: 2)];
  items := [(orderId: 100; sku: 'a')];
  result := [];
  for o in orders do begin
  for c in customers do begin
  for i in items do begin
  if (o.customerId = c.id) and (o.id = i.orderId) then begin
  result := concat(result, [(orderId: o.id; name: c.name; item: i)]);
end;
end;
end;
end;
  writeln('--- Left Join Multi ---');
  for r in result do begin
  writeln(r.orderId, r.name, r.item);
end;
end.
