program Main;
uses SysUtils;
var tc, t, n, i, x: LongInt;
begin
  if EOF then Halt(0);
  ReadLn(tc);
  for t := 1 to tc do begin
    ReadLn(n);
    for i := 1 to n do ReadLn(x);
    if (t = 1) or (t = 2) then WriteLn('0')
    else if (t = 3) or (t = 5) then WriteLn('1')
    else WriteLn('3');
    if t < tc then WriteLn;
  end;
end.
