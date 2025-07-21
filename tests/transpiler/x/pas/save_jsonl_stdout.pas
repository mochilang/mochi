{$mode objfpc}
program Main;
uses SysUtils;
type Anon1 = record
  name: string;
  age: integer;
end;
var
  people: array of Anon1;
begin
  people := [(name: 'Alice'; age: 30), (name: 'Bob'; age: 25)];
  for row in people do begin
  writeln(((((((('{' + '"name": ') + '"') + row.name) + '"') + ', ') + '"age": ') + IntToStr(row.age)) + '}');
end;
end.
