program Main;

uses SysUtils;

var
  t, tc, n, k, i, j, r, min1, min2, idx1, ans: LongInt;
  costs: array of array of LongInt;
  prev, cur: array of LongInt;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 1 to t do
  begin
    ReadLn(n, k);
    SetLength(costs, n);
    for i := 0 to n - 1 do
    begin
      SetLength(costs[i], k);
      for j := 0 to k - 1 do Read(costs[i][j]);
      ReadLn;
    end;
    if n = 0 then
      ans := 0
    else
    begin
      SetLength(prev, k);
      for j := 0 to k - 1 do prev[j] := costs[0][j];
      for r := 1 to n - 1 do
      begin
        min1 := MaxInt;
        min2 := MaxInt;
        idx1 := -1;
        for j := 0 to k - 1 do
        begin
          if prev[j] < min1 then
          begin
            min2 := min1;
            min1 := prev[j];
            idx1 := j;
          end
          else if prev[j] < min2 then
            min2 := prev[j];
        end;
        SetLength(cur, k);
        for j := 0 to k - 1 do
        begin
          if j = idx1 then cur[j] := costs[r][j] + min2
          else cur[j] := costs[r][j] + min1;
        end;
        prev := cur;
      end;
      ans := prev[0];
      for j := 1 to k - 1 do if prev[j] < ans then ans := prev[j];
    end;
    if tc > 1 then WriteLn;
    Write(ans);
  end;
end.
