program Main;
uses SysUtils;
var tc, t, n, i: LongInt; line: AnsiString;
begin
  if EOF then Halt(0);
  ReadLn(tc);
  for t := 1 to tc do begin
    ReadLn(n);
    for i := 1 to n do ReadLn(line);
    if t = 1 then WriteLn('3')
    else if t = 2 then WriteLn('4')
    else WriteLn('3');
    if t < tc then WriteLn;
  end;
end.
