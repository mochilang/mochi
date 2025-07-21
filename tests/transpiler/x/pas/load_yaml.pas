{$mode objfpc}
program Main;
type Person = record
  name: string;
  age: integer;
  email: string;
end;
type Anon1 = record
  name: string;
  email: string;
end;
var
  people: array of Person;
  adults: array of Anon1;
  p: Person;
  a: integer;
begin
  people := [(name: 'Alice'; age: 30; email: 'alice@example.com'), (name: 'Bob'; age: 15; email: 'bob@example.com'), (name: 'Charlie'; age: 20; email: 'charlie@example.com')];
  adults := [];
  for p in people do begin
  if p.age >= 18 then begin
  adults := concat(adults, [(name: p.name; email: p.email)]);
end;
end;
  for a in adults do begin
  writeln(a.name, ' ', a.email);
end;
end.
