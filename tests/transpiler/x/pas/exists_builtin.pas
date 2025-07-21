{$mode objfpc}
program Main;
uses SysUtils;
var
  data: array of integer;
  tmp1: array of integer;
  flag: boolean;
  x: integer;
begin
  data := [1, 2];
  tmp1 := [];
  for x in data do begin
  if x = 1 then begin
  tmp1 := concat(tmp1, [x]);
end;
end;
  flag := Length(tmp1) > 0;
  writeln(BoolToStr(flag, True));
end.
