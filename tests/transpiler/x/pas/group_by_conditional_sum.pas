{$mode objfpc}
program Main;
type Anon1 = record
  cat: string;
  val: integer;
  flag: boolean;
end;
type Anon2 = record
  cat: string;
  sumTrue: integer;
  sumTotal: integer;
end;
type Anon3 = record
  cat: string;
  share: real;
end;
var
  items: array of Anon1;
  grp1: array of Anon2;
  idx2: integer;
  i3: integer;
  result: array of Anon3;
  i: Anon1;
begin
  items := [(cat: 'a'; val: 10; flag: true), (cat: 'a'; val: 5; flag: false), (cat: 'b'; val: 20; flag: true)];
  grp1 := [];
  for i in items do begin
  idx2 := -1;
  for i3 := 0 to (Length(grp1) - 1) do begin
  if grp1[i3].cat = i.cat then begin
  idx2 := i3;
  break;
end;
end;
  if idx2 = -1 then begin
  grp1 := concat(grp1, [(cat: i.cat; sumTrue: IfThen(i.flag, i.val, 0); sumTotal: i.val)]);
end else begin
  if i.flag then begin
  grp1[idx2].sumTrue := grp1[idx2].sumTrue + i.val;
end;
  grp1[idx2].sumTotal := grp1[idx2].sumTotal + i.val;
end;
end;
  result := [];
  for g in grp1 do begin
  result := concat(result, [(cat: g.cat; share: g.sumTrue / g.sumTotal)]);
end;
  writeln(result);
end.
