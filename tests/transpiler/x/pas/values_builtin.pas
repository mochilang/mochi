{$mode objfpc}
program Main;
type Anon1 = record
  a: integer;
  b: integer;
  c: integer;
end;
var
  m: Anon1;
begin
  m := (a: 1; b: 2; c: 3);
  writeln(values(m));
end.
