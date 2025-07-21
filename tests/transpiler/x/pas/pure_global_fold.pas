{$mode objfpc}
program Main;
function inc(x: integer): integer;
begin
  exit(x + k);
end;
var
  k: integer;
begin
  k := 2;
  writeln(inc(3));
end.
