{$mode objfpc}
program Main;
uses SysUtils;
type Anon49 = record
  a: integer;
  b: integer;
end;
var
  m: Anon49;
begin
  m := (a: 1; b: 2);
  writeln(BoolToStr(Pos('a', m) <> 0, True));
  writeln(BoolToStr(Pos('c', m) <> 0, True));
end.
