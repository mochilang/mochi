program Main;
uses SysUtils;
var t, tc: Integer; s1, s2: AnsiString; memo: array[0..30,0..30,0..30] of ShortInt;

function Dfs(i1, i2, len: Integer): Boolean;
var a, b: AnsiString; cnt: array[0..25] of Integer; i, k: Integer;
begin
  if memo[i1, i2, len] <> -1 then exit(memo[i1, i2, len] = 1);
  a := Copy(s1, i1 + 1, len); b := Copy(s2, i2 + 1, len);
  if a = b then begin memo[i1, i2, len] := 1; exit(True); end;
  FillChar(cnt, SizeOf(cnt), 0);
  for i := 1 to len do begin Inc(cnt[Ord(a[i]) - Ord('a')]); Dec(cnt[Ord(b[i]) - Ord('a')]); end;
  for i := 0 to 25 do if cnt[i] <> 0 then begin memo[i1, i2, len] := 0; exit(False); end;
  for k := 1 to len - 1 do
    if (Dfs(i1, i2, k) and Dfs(i1 + k, i2 + k, len - k)) or (Dfs(i1, i2 + len - k, k) and Dfs(i1 + k, i2, len - k)) then begin memo[i1, i2, len] := 1; exit(True); end;
  memo[i1, i2, len] := 0; exit(False);
end;

begin
  if eof(input) then halt;
  readln(t);
  for tc := 0 to t - 1 do begin
    readln(s1); readln(s2); FillChar(memo, SizeOf(memo), 255);
    if Dfs(0, 0, Length(s1)) then write('true') else write('false');
    if tc + 1 < t then writeln;
  end;
end.
