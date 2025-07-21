{$mode objfpc}
program Main;
function twoSum(nums: integer; target: integer): integer;
begin
  n := Length(nums);
  for i := 0 to (n - 1) do begin
  for j := i + 1 to (n - 1) do begin
  if (nums[i] + nums[j]) = target then begin
  exit([i, j]);
end;
end;
end;
  exit([-1, -1]);
end;
var
  n: integer;
  result: array of integer;
  i: integer;
  j: integer;
begin
  n := Length(nums);
  result := twoSum([2, 7, 11, 15], 9);
  writeln(result[0]);
  writeln(result[1]);
end.
