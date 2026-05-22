{$mode objfpc}
program Main;
uses SysUtils;
begin
  writeln(BoolToStr('a' < 'b', True));
  writeln(BoolToStr('a' <= 'a', True));
  writeln(BoolToStr('b' > 'a', True));
  writeln(BoolToStr('b' >= 'b', True));
end.
