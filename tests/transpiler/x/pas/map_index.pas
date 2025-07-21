{$mode objfpc}
program Main;
type Anon47 = record
  a: integer;
  b: integer;
end;
var
  m: Anon47;
begin
  m := (a: 1; b: 2);
  writeln(m['b']);
end.
