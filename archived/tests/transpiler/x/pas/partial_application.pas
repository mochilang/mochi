{$mode objfpc}
program Main;
function add(a: integer; b: integer): integer;
begin
  exit(a + b);
end;
var
  add5: integer;
begin
  add5 := add(5);
  writeln(add5(3));
end.
