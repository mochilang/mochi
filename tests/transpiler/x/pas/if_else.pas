{$mode objfpc}
program Main;
var
  x: integer;
begin
  x := 5;
  if x > 3 then begin
  writeln('big');
end else begin
  writeln('small');
end;
end.
