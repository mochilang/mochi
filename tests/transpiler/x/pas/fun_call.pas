{$mode objfpc}
program Main;
uses SysUtils;
function add(a: integer; b: integer): integer;
begin
  exit(a + b);
end;
begin
  writeln(add(2, 3));
end.
