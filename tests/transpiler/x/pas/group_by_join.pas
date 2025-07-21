{$mode objfpc}
program Main;
type Anon18 = record
  id: integer;
  name: string;
end;
type Anon19 = record
  id: integer;
  customerId: integer;
end;
type Anon20 = record
  name: string;
  count: integer;
end;
var
  customers: array of Anon18;
  orders: array of Anon19;
  grp2: array of Anon20;
  idx3: integer;
  i4: integer;
  stats: array of Anon20;
  o: Anon19;
  c: Anon18;
  s: integer;
begin
  customers := [(id: 1; name: 'Alice'), (id: 2; name: 'Bob')];
  orders := [(id: 100; customerId: 1), (id: 101; customerId: 1), (id: 102; customerId: 2)];
  grp2 := [];
  for o in orders do begin
  for c in customers do begin
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
  writeln('--- Orders per customer ---');
  for s in stats do begin
  writeln(s.name, 'orders:', s.count);
end;
end.
