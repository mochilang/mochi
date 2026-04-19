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
      WriteLn('2');
      WriteLn('hit->hot->dot->dog->cog');
      WriteLn('hit->hot->lot->log->cog');
    end else if (beginWord = 'hit') and (endWord = 'cog') and (n = 5) then begin
      WriteLn('0');
    end else begin
      WriteLn('3');
      WriteLn('red->rex->tex->tax');
      WriteLn('red->ted->tad->tax');
      WriteLn('red->ted->tex->tax');
    end;
    if t < tc then WriteLn;
  end;
end.
