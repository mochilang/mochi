program Main;
uses SysUtils;
function GetPermutation(n, kInput: LongInt): AnsiString;
var digits: array[0..8] of AnsiString; fact: array[0..9] of LongInt; k, len, rem, idx, i: LongInt; res: AnsiString;
begin
  for i := 0 to n - 1 do digits[i] := IntToStr(i + 1);
  fact[0] := 1;
  for i := 1 to n do fact[i] := fact[i - 1] * i;
  k := kInput - 1;
  len := n;
  res := '';
  for rem := n downto 1 do begin
    idx := k div fact[rem - 1];
    k := k mod fact[rem - 1];
    res := res + digits[idx];
    for i := idx to len - 2 do digits[i] := digits[i + 1];
    len := len - 1;
  end;
  GetPermutation := res;
end;
var t, tc, n, k: LongInt;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 1 to t do begin
    ReadLn(n);
    ReadLn(k);
    Write(GetPermutation(n, k));
    if tc < t then WriteLn;
  end;
end.
