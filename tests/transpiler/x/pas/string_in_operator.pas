{$mode objfpc}
program Main;
uses SysUtils;
var
  s: string;
begin
  s := 'catch';
  writeln(BoolToStr(Pos('cat', s) <> 0, True));
  writeln(BoolToStr(Pos('dog', s) <> 0, True));
end.
