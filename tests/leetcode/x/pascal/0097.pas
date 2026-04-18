program Main;
uses SysUtils;
var t, tc, i, j, m, n: Integer; s1, s2, s3: AnsiString; dp: array[0..110,0..110] of Boolean;
begin
  if eof(input) then halt;
  readln(t);
  for tc := 0 to t - 1 do
  begin
    readln(s1); readln(s2); readln(s3);
    m := Length(s1); n := Length(s2);
    FillChar(dp, SizeOf(dp), 0);
    if m + n = Length(s3) then
    begin
      dp[0,0] := True;
      for i := 0 to m do
        for j := 0 to n do
        begin
          if (i > 0) and dp[i-1,j] and (s1[i] = s3[i+j]) then dp[i,j] := True;
          if (j > 0) and dp[i,j-1] and (s2[j] = s3[i+j]) then dp[i,j] := True;
        end;
      if dp[m,n] then write('true') else write('false');
    end else write('false');
    if tc + 1 < t then writeln;
  end;
end.
