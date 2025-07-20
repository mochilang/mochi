{$mode objfpc}
program Main;
uses StrUtils;
function sum3(a: integer; b: integer; c: integer): integer;
begin
  exit((a + b) + c);
end;
begin
  writeln(sum3(1, 2, 3));
end.
