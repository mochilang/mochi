{$mode objfpc}
program Main;
var
  a: integer;
  b: integer;
begin
  a := 10 - 3;
  b := 2 + 2;
  writeln(a);
  writeln(ord(a = 7));
  writeln(ord(b < 5));
end.
