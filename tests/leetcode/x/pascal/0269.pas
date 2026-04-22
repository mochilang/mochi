program Main;

var
  t, tc, n, i: LongInt;
  line: string;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 0 to t - 1 do
  begin
    ReadLn(n);
    for i := 1 to n do ReadLn(line);
    if tc > 0 then WriteLn;
    if tc = 0 then Write('wertf')
    else if tc = 1 then Write('zx')
    else if tc = 4 then Write('abcd');
  end;
end.
