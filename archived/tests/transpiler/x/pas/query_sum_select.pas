{$mode objfpc}
program Main;
var
  nums: array of integer;
  result: array of ;
  n: integer;
begin
  nums := [1, 2, 3];
  result := [];
  for n in nums do begin
  if n > 1 then begin
  result := concat(result, [sum(n)]);
end;
end;
  writeln(result);
end.
