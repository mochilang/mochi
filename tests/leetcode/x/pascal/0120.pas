program Main;
var t, tc, rows, r, j: Integer; tri: array[0..255,0..255] of Integer; dp: array[0..255] of Integer;
begin
  if eof(input) then halt;
  readln(t);
  for tc := 0 to t - 1 do
  begin
    readln(rows);
    for r := 0 to rows - 1 do
      for j := 0 to r do
        readln(tri[r,j]);
    for j := 0 to rows - 1 do dp[j] := tri[rows - 1, j];
    for r := rows - 2 downto 0 do
      for j := 0 to r do
        if dp[j] < dp[j + 1] then dp[j] := tri[r,j] + dp[j] else dp[j] := tri[r,j] + dp[j + 1];
    write(dp[0]);
    if tc + 1 < t then writeln;
  end;
end.
