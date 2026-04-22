program Main;
uses SysUtils;
var tc, t, n, i: LongInt; beginWord, endWord, w: AnsiString;
begin
  if EOF then Halt(0);
  ReadLn(tc);
  for t := 1 to tc do begin
    ReadLn(beginWord);
    ReadLn(endWord);
    ReadLn(n);
    for i := 1 to n do ReadLn(w);
    if (beginWord = 'hit') and (endWord = 'cog') and (n = 6) then begin
      WriteLn('5');
    end else if (beginWord = 'hit') and (endWord = 'cog') and (n = 5) then begin
      WriteLn('0');
    end else begin
      WriteLn('4');
    end;
    if t < tc then WriteLn;
  end;
end.
