program Main;
var
  t, tc, rows, cols, r, c, i, j, mn, area, best: Integer;
  s: AnsiString;
  h: array[0..255] of Integer;
begin
  if eof(input) then halt;
  readln(t);
  for tc := 0 to t - 1 do
  begin
    readln(rows, cols);
    for c := 0 to cols - 1 do h[c] := 0;
    best := 0;
    for r := 0 to rows - 1 do
    begin
      readln(s);
      for c := 1 to cols do
        if s[c] = '1' then h[c - 1] := h[c - 1] + 1 else h[c - 1] := 0;
      for i := 0 to cols - 1 do
      begin
        mn := h[i];
        for j := i to cols - 1 do
        begin
          if h[j] < mn then mn := h[j];
          area := mn * (j - i + 1);
          if area > best then best := area;
        end;
      end;
    end;
    write(best);
    if tc + 1 < t then writeln;
  end;
end.
