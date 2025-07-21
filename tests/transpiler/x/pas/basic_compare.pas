{$mode objfpc}
program Main;
uses SysUtils;
var
  a: integer;
  b: integer;
begin
  a := 10 - 3;
  b := 2 + 2;
  writeln(a);
  writeln(BoolToStr(a = 7, True));
  writeln(BoolToStr(b < 5, True));
end.
