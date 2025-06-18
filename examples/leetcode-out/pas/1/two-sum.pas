program main;
{$mode objfpc}
uses SysUtils;

type TIntArray = array of integer;

function twoSum(nums: TIntArray; target: integer): TIntArray;
var
	i: integer;
	j: integer;
	n: integer;
begin
	n := Length(nums);
	for i := 0 to n - 1 do
	begin
		for j := i + 1 to n - 1 do
		begin
			if (nums[i] + nums[j] = target) then
			begin
				result := TIntArray([i, j]);
				exit;
			end;
		end;
	end;
	result := TIntArray([-1, -1]);
	exit;
end;

var
	_result: TIntArray;

begin
	_result := twoSum(TIntArray([2, 7, 11, 15]), 9);
	writeln(_result[0]);
	writeln(_result[1]);
end.
