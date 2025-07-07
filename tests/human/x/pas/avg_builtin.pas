program AvgBuiltin;
var
  arr: array[1..3] of integer = (1,2,3);
  sum,i: integer;
  avg: real;
begin
  sum := 0;
  for i := 1 to 3 do sum := sum + arr[i];
  avg := sum / 3;
  if Frac(avg) = 0 then
    Writeln(Trunc(avg))
  else
    Writeln(avg:0:1);
end.
