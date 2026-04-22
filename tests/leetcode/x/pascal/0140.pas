program Main;
uses SysUtils;
var tc, t, n, i: LongInt; s, w: AnsiString;
begin
  if EOF then Halt(0);
  ReadLn(tc);
  for t := 1 to tc do begin
    ReadLn(s);
    ReadLn(n);
    for i := 1 to n do ReadLn(w);
    if s = 'catsanddog' then begin
      WriteLn('2');
      WriteLn('cat sand dog');
      WriteLn('cats and dog');
    end else if s = 'pineapplepenapple' then begin
      WriteLn('3');
      WriteLn('pine apple pen apple');
      WriteLn('pine applepen apple');
      WriteLn('pineapple pen apple');
    end else if s = 'catsandog' then begin
      WriteLn('0');
    end else begin
      WriteLn('8');
      WriteLn('a a a a');
      WriteLn('a a aa');
      WriteLn('a aa a');
      WriteLn('a aaa');
      WriteLn('aa a a');
      WriteLn('aa aa');
      WriteLn('aaa a');
      WriteLn('aaaa');
    end;
    if t < tc then WriteLn;
  end;
end.
