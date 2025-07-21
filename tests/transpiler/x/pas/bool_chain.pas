{$mode objfpc}
program Main;
uses SysUtils;
function boom(): boolean;
begin
  writeln('boom');
  exit(true);
end;
begin
  writeln(BoolToStr(((1 < 2) and (2 < 3)) and (3 < 4), True));
  writeln(BoolToStr(((1 < 2) and (2 > 3)) and boom(), True));
  writeln(BoolToStr((((1 < 2) and (2 < 3)) and (3 > 4)) and boom(), True));
end.
