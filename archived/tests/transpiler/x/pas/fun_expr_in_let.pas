{$mode objfpc}
program Main;
function anon0(x: integer): integer;
begin
  exit(x * x);
end;
var
  square: integer;
begin
  square := anon0;
  writeln(square(6));
end.
