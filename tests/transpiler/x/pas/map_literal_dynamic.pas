{$mode objfpc}
program Main;
type Anon48 = record
  a: ;
  b: ;
end;
var
  x: integer;
  y: integer;
  m: Anon48;
begin
  x := 3;
  y := 4;
  m := (a: x; b: y);
  writeln(m['a'], m['b']);
end.
