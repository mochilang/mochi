{$mode objfpc}
program Main;
function triple(x: integer): integer;
begin
  exit(x * 3);
end;
begin
  writeln(triple(1 + 2));
end.
