{$mode objfpc}
program Main;
uses SysUtils;
var
  nums: array of integer;
begin
  nums := [1, 2];
  nums[1] := 3;
  writeln(nums[1]);
end.
