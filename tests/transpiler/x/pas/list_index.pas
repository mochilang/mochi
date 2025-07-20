{$mode objfpc}
program Main;
uses StrUtils;
var
  xs: array of integer;
begin
  xs := [10, 20, 30];
  writeln(xs[1]);
end.
