program ListAssign;
var
  nums: array[0..1] of integer;
begin
  nums[0] := 1;
  nums[1] := 2;
  nums[1] := 3;
  Writeln(nums[1]);
end.
