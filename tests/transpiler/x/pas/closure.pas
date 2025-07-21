{$mode objfpc}
program Main;
function anon0(x: integer): integer;
begin
  exit(x + n);
end;
function makeAdder(n: integer): integer;
begin
  exit(anon0);
end;
var
  add10: integer;
begin
  add10 := makeAdder(10);
  writeln(add10(7));
end.
