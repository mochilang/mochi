program Main;

function Solve(s, t: AnsiString): LongInt;
var n, i, j: LongInt; dp: array[0..1000] of LongInt;
begin
  n := Length(t);
  for j := 0 to n do dp[j] := 0;
  dp[0] := 1;
  for i := 1 to Length(s) do
    for j := n downto 1 do
      if s[i] = t[j] then dp[j] := dp[j] + dp[j - 1];
  Solve := dp[n];
end;

var tc, i: LongInt; s, t: AnsiString;
begin
  if EOF then Halt(0);
  ReadLn(tc);
  for i := 0 to tc - 1 do begin
    ReadLn(s);
    ReadLn(t);
    Write(Solve(s, t));
    if i + 1 < tc then WriteLn;
  end;
end.
