program TwoSum;

type
  TIntArray = array of integer;

function twoSum(nums: TIntArray; target: integer): TIntArray;
var
  n,i,j: integer;
begin
  n := Length(nums);
  SetLength(Result, 2);
  for i := 0 to n-1 do
    for j := i+1 to n-1 do
      if nums[i] + nums[j] = target then
      begin
        Result[0] := i;
        Result[1] := j;
        Exit;
      end;
  Result[0] := -1;
  Result[1] := -1;
end;

var
  arr,res: TIntArray;
begin
  arr := TIntArray.Create(2,7,11,15);
  res := twoSum(arr, 9);
  Writeln(res[0]);
  Writeln(res[1]);
end.
