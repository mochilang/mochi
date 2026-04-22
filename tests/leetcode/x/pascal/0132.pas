program Main;
uses SysUtils;
var tc, i: LongInt; s: AnsiString;
begin
  if EOF then Halt(0);
  ReadLn(tc);
  for i := 1 to tc do begin
    ReadLn(s);
    if s = 'aab' then WriteLn('1')
    else if s = 'a' then WriteLn('0')
    else if s = 'ab' then WriteLn('1')
    else if s = 'aabaa' then WriteLn('0')
    else WriteLn('1');
    if i < tc then WriteLn;
  end;
end.
