{$mode objfpc}
program Main;
type Anon10 = record
  name: string;
  age: integer;
  city: string;
end;
type Anon11 = record
  city: string;
  count: integer;
  sumAge: integer;
end;
type Anon12 = record
  city: string;
  count: integer;
  avg_age: real;
end;
var
  people: array of Anon10;
  grp1: array of Anon11;
  idx2: integer;
  i3: integer;
  sum4: integer;
  stats: array of Anon12;
  person: Anon10;
  s: integer;
begin
  people := [(name: 'Alice'; age: 30; city: 'Paris'), (name: 'Bob'; age: 15; city: 'Hanoi'), (name: 'Charlie'; age: 65; city: 'Paris'), (name: 'Diana'; age: 45; city: 'Hanoi'), (name: 'Eve'; age: 70; city: 'Paris'), (name: 'Frank'; age: 22; city: 'Hanoi')];
  grp1 := [];
  for person in people do begin
  idx2 := -1;
  for i3 := 0 to (Length(grp1) - 1) do begin
  if grp1[i3].city = person.city then begin
  idx2 := i3;
  break;
end;
end;
  if idx2 = -1 then begin
  grp1 := concat(grp1, [(city: person.city; count: 1; sumAge: person.age)]);
end else begin
  grp1[idx2].count := grp1[idx2].count + 1;
  grp1[idx2].sumAge := grp1[idx2].sumAge + person.age;
end;
end;
  stats := [];
  for g in grp1 do begin
  sum4 := 0;
  for person in g.items do begin
  sum4 := sum4 + person.age;
end;
  stats := concat(stats, [(city: g.city; count: g.count; avg_age: sum4 / Length(g.items))]);
end;
  writeln('--- People grouped by city ---');
  for s in stats do begin
  writeln(s.city, ': count =', s.count, ', avg_age =', s.avg_age);
end;
end.
