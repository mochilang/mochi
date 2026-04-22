program Main;

uses SysUtils, Math;

var
  t, tc, n, k, i, rightIdx, leftIdx, pick: LongInt;
  target: Double;
  values: array of LongInt;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 1 to t do
  begin
    ReadLn(n);
    SetLength(values, n);
    for i := 0 to n - 1 do ReadLn(values[i]);
    ReadLn(target);
    ReadLn(k);
    rightIdx := 0;
    while (rightIdx < n) and (values[rightIdx] < target) do Inc(rightIdx);
    leftIdx := rightIdx - 1;
    if tc > 1 then WriteLn;
    WriteLn(k);
    for i := 1 to k do
    begin
      if leftIdx < 0 then
      begin
        pick := values[rightIdx];
        Inc(rightIdx);
      end
      else if rightIdx >= n then
      begin
        pick := values[leftIdx];
        Dec(leftIdx);
      end
      else if Abs(values[leftIdx] - target) <= Abs(values[rightIdx] - target) then
      begin
        pick := values[leftIdx];
        Dec(leftIdx);
      end
      else
      begin
        pick := values[rightIdx];
        Inc(rightIdx);
      end;
      Write(pick);
      if i < k then WriteLn;
    end;
    if tc < t then WriteLn;
  end;
end.
