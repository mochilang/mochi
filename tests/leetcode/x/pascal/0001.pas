program TwoSum;

var
  t, tc, n, target, i, j, a, b: LongInt;
  nums: array of LongInt;

begin
  if EOF then
    Halt(0);
  Read(t);
  for tc := 1 to t do
  begin
    Read(n);
    Read(target);
    SetLength(nums, n);
    for i := 0 to n - 1 do
      Read(nums[i]);
    a := 0;
    b := 0;
    for i := 0 to n - 1 do
    begin
      for j := i + 1 to n - 1 do
      begin
        if nums[i] + nums[j] = target then
        begin
          a := i;
          b := j;
          Break;
        end;
      end;
      if nums[a] + nums[b] = target then
        Break;
    end;
    WriteLn(a, ' ', b);
  end;
end.
