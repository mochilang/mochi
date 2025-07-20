{$mode objfpc}
program Main;
uses StrUtils;
begin
  writeln(1 + (2 * 3));
  writeln((1 + 2) * 3);
  writeln((2 * 3) + 1);
  writeln(2 * (3 + 1));
end.
