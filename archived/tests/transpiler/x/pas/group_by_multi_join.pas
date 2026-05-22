{$mode objfpc}
program Main;
type Anon1 = record
  id: integer;
  name: string;
end;
type Anon2 = record
  id: integer;
  nation: integer;
end;
type Anon3 = record
  part: integer;
  supplier: integer;
  cost: real;
  qty: integer;
end;
type Anon4 = record
  part: integer;
  value: real;
end;
type Anon5 = record
  part: integer;
  total: real;
end;
var
  nations: array of Anon1;
  suppliers: array of Anon2;
  partsupp: array of Anon3;
  filtered: array of Anon4;
  grp4: array of Anon5;
  idx5: integer;
  i6: integer;
  grouped: array of Anon5;
  ps: Anon3;
  s: Anon2;
  n: Anon1;
  x: Anon4;
begin
  nations := [(id: 1; name: 'A'), (id: 2; name: 'B')];
  suppliers := [(id: 1; nation: 1), (id: 2; nation: 2)];
  partsupp := [(part: 100; supplier: 1; cost: 10; qty: 2), (part: 100; supplier: 2; cost: 20; qty: 1), (part: 200; supplier: 1; cost: 5; qty: 3)];
  filtered := [];
  for ps in partsupp do begin
  for s in suppliers do begin
  for n in nations do begin
  if ((s.id = ps.supplier) and (n.id = s.nation)) and (n.name = 'A') then begin
  filtered := concat(filtered, [(part: ps.part; value: ps.cost * ps.qty)]);
end;
end;
end;
end;
  grp4 := [];
  for x in filtered do begin
  idx5 := -1;
  for i6 := 0 to (Length(grp4) - 1) do begin
  if grp4[i6].part = x.part then begin
  idx5 := i6;
  break;
end;
end;
  if idx5 = -1 then begin
  grp4 := concat(grp4, [(part: x.part; total: x.value)]);
end else begin
  grp4[idx5].total := grp4[idx5].total + x.value;
end;
end;
  grouped := grp4;
  writeln(grouped);
end.
