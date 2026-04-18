program Main;
var
  t, tc, n, i, j, mn, area, best, ans: Integer;
  a: array of Integer;
begin
  if eof(input) then halt;
  readln(t);
  for tc := 0 to t - 1 do
  begin
    readln(n);
    SetLength(a, n);
    for i := 0 to n - 1 do
      readln(a[i]);
    best := 0;
    for i := 0 to n - 1 do
    begin
      mn := a[i];
      for j := i to n - 1 do
      begin
        if a[j] < mn then mn := a[j];
        area := mn * (j - i + 1);
        if area > best then best := area;
      end;
    end;
    write(best);
    if tc + 1 < t then writeln;
  end;
end.
