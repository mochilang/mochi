{$mode objfpc}
program Main;
type Anon61 = record
  a: integer;
  b: integer;
  c: integer;
end;
var
  m: Anon61;
begin
  m := (a: 1; b: 2; c: 3);
  writeln(values(m));
end.
