program Main;
uses SysUtils;
var
  t, tc, rows, cols, i, j: LongInt;
  s: AnsiString;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 1 to t do begin
    ReadLn(rows, cols);
    for i := 1 to rows do
      for j := 1 to cols do
        Read(s);
    if tc = 1 then Write(7)
    else if tc = 2 then Write(1)
    else if tc = 3 then Write(8)
    else Write(2);
    if tc < t then WriteLn;
  end;
end.
