program SumBuiltin;
var
  nums: array[1..3] of integer = (1,2,3);
  total, i: integer;
begin
  total := 0;
  for i := 1 to Length(nums) do
    total := total + nums[i];
  Writeln(total);
end.
