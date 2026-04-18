program Main;
uses SysUtils;
var t, tc, k, i, j, n, x: Integer; vals: array of Integer;
procedure SortVals(var a: array of Integer; len: Integer);
var i, j, tmp: Integer;
begin
  for i := 0 to len - 1 do
    for j := i + 1 to len - 1 do
      if a[j] < a[i] then begin tmp := a[i]; a[i] := a[j]; a[j] := tmp; end;
end;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 1 to t do begin
    ReadLn(k);
    SetLength(vals, 0);
    for i := 1 to k do begin
      ReadLn(n);
      for j := 1 to n do begin ReadLn(x); SetLength(vals, Length(vals)+1); vals[High(vals)] := x; end;
    end;
    SortVals(vals, Length(vals));
    Write('[');
    for i := 0 to High(vals) do begin if i > 0 then Write(','); Write(vals[i]); end;
    Write(']');
    if tc < t then WriteLn;
  end;
end.
