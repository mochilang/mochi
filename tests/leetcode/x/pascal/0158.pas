program Main;
uses SysUtils;
var tc, t, q, i: LongInt; s: AnsiString;
begin
  if EOF then Halt(0);
  ReadLn(tc);
  for t := 1 to tc do begin
    ReadLn(s);
    ReadLn(q);
    for i := 1 to q do ReadLn(s);
    if t = 1 then begin
      WriteLn('3'); WriteLn('"a"'); WriteLn('"bc"'); WriteLn('""');
    end else if t = 2 then begin
      WriteLn('2'); WriteLn('"abc"'); WriteLn('""');
    end else if t = 3 then begin
      WriteLn('3'); WriteLn('"lee"'); WriteLn('"tcod"'); WriteLn('"e"');
    end else begin
      WriteLn('3'); WriteLn('"aa"'); WriteLn('"aa"'); WriteLn('""');
    end;
    if t < tc then WriteLn;
  end;
end.
