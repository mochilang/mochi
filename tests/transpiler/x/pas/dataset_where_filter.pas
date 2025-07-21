{$mode objfpc}
program Main;
uses SysUtils;
type Anon7 = record
  name: string;
  age: integer;
end;
type Anon8 = record
  name: string;
  age: integer;
  is_senior: boolean;
end;
var
  people: array of Anon7;
  adults: array of Anon8;
  person: Anon7;
begin
  people := [(name: 'Alice'; age: 30), (name: 'Bob'; age: 15), (name: 'Charlie'; age: 65), (name: 'Diana'; age: 45)];
  adults := [];
  for person in people do begin
  if person.age >= 18 then begin
  adults := concat(adults, [(name: person.name; age: person.age; is_senior: person.age >= 60)]);
end;
end;
  writeln('--- Adults ---');
  for person in adults do begin
  writeln(person.name, 'is', person.age, IfThen(person.is_senior, ' (senior)', ''));
end;
end.
