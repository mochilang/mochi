program Main;
uses SysUtils;
var t, tc, n, k, i, l, r, tmp, start: Integer; arr: array of Integer;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 1 to t do begin
    ReadLn(n);
    SetLength(arr, n);
    for i := 0 to n - 1 do ReadLn(arr[i]);
    ReadLn(k);
    start := 0;
    while start + k <= n do begin
      l := start; r := start + k - 1;
      while l < r do begin tmp := arr[l]; arr[l] := arr[r]; arr[r] := tmp; Inc(l); Dec(r); end;
      start := start + k;
    end;
    Write('[');
    for i := 0 to n - 1 do begin if i > 0 then Write(','); Write(arr[i]); end;
    Write(']');
    if tc < t then WriteLn;
  end;
end.
