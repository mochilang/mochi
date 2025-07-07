program BreakContinue;
var
  numbers: array[1..9] of integer = (1,2,3,4,5,6,7,8,9);
  i: integer;
begin
  for i := 1 to 9 do
  begin
    if numbers[i] mod 2 = 0 then
      continue;
    if numbers[i] > 7 then
      break;
    Writeln('odd number: ', numbers[i]);
  end;
end.
