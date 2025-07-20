{$mode objfpc}
program Main;
var
  a: array of integer;
begin
  a := [1, 2];
  writeln(concat(a, [3]));
end.
