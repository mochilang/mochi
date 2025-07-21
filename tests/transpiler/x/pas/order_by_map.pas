{$mode objfpc}
program Main;
type Anon52 = record
  a: integer;
  b: integer;
end;
var
  data: array of Anon52;
  sorted: array of Anon52;
  x: Anon52;
begin
  data := [(a: 1; b: 2), (a: 1; b: 1), (a: 0; b: 5)];
  sorted := [];
  for x in data do begin
  sorted := concat(sorted, [x]);
end;
  writeln(sorted);
end.
