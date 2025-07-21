{$mode objfpc}
program Main;
var
  numbers: array of integer;
  n: integer;
begin
  numbers := [1, 2, 3, 4, 5, 6, 7, 8, 9];
  for n in numbers do begin
  if (n mod 2) = 0 then begin
  continue;
end;
  if n > 7 then begin
  break;
end;
  writeln('odd number:', ' ', n);
end;
end.
