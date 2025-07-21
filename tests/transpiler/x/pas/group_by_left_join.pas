{$mode objfpc}
program Main;
type Anon21 = record
  id: integer;
  name: string;
end;
type Anon22 = record
  id: integer;
  customerId: integer;
end;
type Anon23 = record
  name: string;
  count: integer;
end;
var
  customers: array of Anon21;
  orders: array of Anon22;
  grp2: array of Anon23;
  idx3: integer;
  i4: integer;
  stats: array of Anon23;
  o: Anon22;
  s: integer;
  c: Anon21;
begin
  customers := [(id: 1; name: 'Alice'), (id: 2; name: 'Bob'), (id: 3; name: 'Charlie')];
  orders := [(id: 100; customerId: 1), (id: 101; customerId: 1), (id: 102; customerId: 2)];
  grp2 := [];
  for c in customers do begin
  for o in orders do begin
  if o.customerId = c.id then begin
  idx3 := -1;
  for i4 := 0 to (Length(grp2) - 1) do begin
  if grp2[i4].name = c.name then begin
  idx3 := i4;
  break;
end;
end;
  if idx3 = -1 then begin
  grp2 := concat(grp2, [(name: c.name; count: 1)]);
end else begin
  grp2[idx3].count := grp2[idx3].count + 1;
end;
end;
end;
end;
  stats := grp2;
  writeln('--- Group Left Join ---');
  for s in stats do begin
  writeln(s.name, 'orders:', s.count);
end;
end.
