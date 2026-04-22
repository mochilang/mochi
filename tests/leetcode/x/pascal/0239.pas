program Main;

uses SysUtils;

var
  t, tc, n, k, i, head, tail, countAns: LongInt;
  nums, dq, ans: array of LongInt;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 1 to t do
  begin
    ReadLn(n);
    SetLength(nums, n);
    for i := 0 to n - 1 do ReadLn(nums[i]);
    ReadLn(k);
    SetLength(dq, n);
    SetLength(ans, n - k + 1);
    head := 0;
    tail := 0;
    countAns := 0;
    for i := 0 to n - 1 do
    begin
      while (head < tail) and (dq[head] <= i - k) do Inc(head);
      while (head < tail) and (nums[dq[tail - 1]] <= nums[i]) do Dec(tail);
      dq[tail] := i;
      Inc(tail);
      if i >= k - 1 then
      begin
        ans[countAns] := nums[dq[head]];
        Inc(countAns);
      end;
    end;
    if tc > 1 then WriteLn; 
    WriteLn(countAns);
    for i := 0 to countAns - 1 do
    begin
      Write(ans[i]);
      if i + 1 < countAns then WriteLn;
    end;
    if tc < t then WriteLn;
  end;
end.
