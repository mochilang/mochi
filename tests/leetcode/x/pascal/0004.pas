program Main;

function Median(a: array of LongInt; b: array of LongInt): Double;
var m: array of LongInt; i, j, k: LongInt;
begin
  SetLength(m, Length(a) + Length(b)); i := 0; j := 0; k := 0;
  while (i < Length(a)) and (j < Length(b)) do
    if a[i] <= b[j] then begin m[k] := a[i]; Inc(i); Inc(k); end else begin m[k] := b[j]; Inc(j); Inc(k); end;
  while i < Length(a) do begin m[k] := a[i]; Inc(i); Inc(k); end;
  while j < Length(b) do begin m[k] := b[j]; Inc(j); Inc(k); end;
  if k mod 2 = 1 then Median := m[k div 2] else Median := (m[k div 2 - 1] + m[k div 2]) / 2.0;
end;

var t, tc, n, m, i: LongInt; a, b: array of LongInt;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 1 to t do begin
    ReadLn(n); SetLength(a, n); for i := 0 to n - 1 do ReadLn(a[i]);
    ReadLn(m); SetLength(b, m); for i := 0 to m - 1 do ReadLn(b[i]);
    Write(Median(a,b):0:1);
    if tc < t then WriteLn;
  end;
end.
