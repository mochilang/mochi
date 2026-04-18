program Main;
uses SysUtils;

type TIntArray = array of LongInt;

function FirstMissingPositive(var nums: TIntArray): LongInt;
var n, i, v, tmp: LongInt;
begin
  n := Length(nums);
  i := 0;
  while i < n do
  begin
    v := nums[i];
    if (v >= 1) and (v <= n) and (nums[v - 1] <> v) then
    begin
      tmp := nums[i];
      nums[i] := nums[v - 1];
      nums[v - 1] := tmp;
    end
    else
      i := i + 1;
  end;
  for i := 0 to n - 1 do
    if nums[i] <> i + 1 then
    begin
      FirstMissingPositive := i + 1;
      Exit;
    end;
  FirstMissingPositive := n + 1;
end;

var t, tc, n, i: LongInt; nums: TIntArray;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 1 to t do
  begin
    ReadLn(n);
    SetLength(nums, n);
    for i := 0 to n - 1 do ReadLn(nums[i]);
    if tc > 1 then WriteLn;
    Write(FirstMissingPositive(nums));
  end;
end.
