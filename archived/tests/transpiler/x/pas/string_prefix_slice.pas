{$mode objfpc}
program Main;
uses SysUtils;
var
  prefix: string;
  s1: string;
  s2: string;
begin
  prefix := 'fore';
  s1 := 'forest';
  s2 := 'desert';
  writeln(BoolToStr(copy(s1, 0+1, (Length(prefix) - (0))) = prefix, True));
  writeln(BoolToStr(copy(s2, 0+1, (Length(prefix) - (0))) = prefix, True));
end.
