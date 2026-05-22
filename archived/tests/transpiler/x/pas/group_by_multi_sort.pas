{$mode objfpc}
program Main;
type Anon1 = record
  a: string;
  b: integer;
  val: integer;
end;
type Anon2 = record
  a: string;
  b: integer;
end;
type Anon3 = record
  key: Anon2;
  items: array of Anon1;
end;
type Anon4 = record
  a: string;
  b: integer;
  total: integer;
end;
var
  items: array of Anon1;
  grp1: array of Anon3;
  idx2: integer;
  i3: integer;
  sum4: integer;
  i5: integer;
  j6: integer;
  tmp7: Anon4;
  grouped: array of Anon4;
  i: Anon1;
begin
  items := [Anon1(a: 'x'; b: 1; val: 2), Anon1(a: 'x'; b: 2; val: 3), Anon1(a: 'y'; b: 1; val: 4), Anon1(a: 'y'; b: 2; val: 1)];
  grp1 := [];
  for i in items do begin
  idx2 := -1;
  for i3 := 0 to (Length(grp1) - 1) do begin
  if (grp1[i3].key.a = i.a) and (grp1[i3].key.b = i.b) then begin
  idx2 := i3;
  break;
end;
end;
  if idx2 = -1 then begin
  grp1 := concat(grp1, [Anon3(key: Anon2(a: i.a; b: i.b); items: [i])]);
end else begin
  grp1[idx2].items := concat(grp1[idx2].items, [i]);
end;
end;
  grouped := [];
  for g in grp1 do begin
  sum4 := 0;
  for x in g.items do begin
  sum4 := sum4 + x.val;
end;
  grouped := concat(grouped, [Anon4(a: g.key.a; b: g.key.b; total: sum4)]);
end;
  for i5 := 0 to (Length(grouped) - 1 - 1) do begin
  for j6 := i5 + 1 to (Length(grouped) - 1) do begin
  if grouped[i5].total < grouped[j6].total then begin
  tmp7 := grouped[i5];
  grouped[i5] := grouped[j6];
  grouped[j6] := tmp7;
end;
end;
end;
  writeln(grouped);
end.
