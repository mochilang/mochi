program MinMaxBuiltin;
var
  nums: array[0..2] of integer = (3,1,4);
  i, minVal, maxVal: integer;
begin
  minVal := nums[0];
  maxVal := nums[0];
  for i := 1 to 2 do
  begin
    if nums[i] < minVal then minVal := nums[i];
    if nums[i] > maxVal then maxVal := nums[i];
  end;
  Writeln(minVal);
  Writeln(maxVal);
end.
