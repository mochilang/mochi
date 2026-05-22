{$mode objfpc}
program Main;
uses SysUtils;
function boom(a: integer; b: integer): boolean;
begin
  writeln('boom');
  exit(true);
end;
begin
  writeln(BoolToStr(false and boom(1, 2), True));
  writeln(BoolToStr(true or boom(1, 2), True));
end.
