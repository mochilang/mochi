program AddTwoNumbers;
var t, tc, n, m, i, j, k, carry, sum: LongInt; a, b, outv: array[1..256] of LongInt;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 1 to t do begin
    ReadLn(n);
    for i := 1 to n do Read(a[i]); if n > 0 then ReadLn;
    ReadLn(m);
    for i := 1 to m do Read(b[i]); if m > 0 then ReadLn;
    i := 1; j := 1; k := 0; carry := 0;
    while (i <= n) or (j <= m) or (carry > 0) do begin
      sum := carry;
      if i <= n then begin sum := sum + a[i]; Inc(i); end;
      if j <= m then begin sum := sum + b[j]; Inc(j); end;
      Inc(k); outv[k] := sum mod 10; carry := sum div 10;
    end;
    Write('[');
    for i := 1 to k do begin if i > 1 then Write(','); Write(outv[i]); end;
    WriteLn(']');
  end;
end.
