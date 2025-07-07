program QuerySumSelect;
var
  nums: array[0..2] of integer = (1,2,3);
  i, sum: integer;
begin
  sum := 0;
  for i := 0 to 2 do
    if nums[i] > 1 then
      sum := sum + nums[i];
  Writeln(sum);
end.
