program Main;
uses SysUtils;
function IsMatch(s, p: AnsiString): Boolean;
var i,j,starPos,matchPos: LongInt;
begin
  i := 1; j := 1; starPos := -1; matchPos := 1;
  while i <= Length(s) do begin
    if (j <= Length(p)) and ((p[j] = '?') or (p[j] = s[i])) then begin Inc(i); Inc(j); end
    else if (j <= Length(p)) and (p[j] = '*') then begin starPos := j; matchPos := i; Inc(j); end
    else if starPos <> -1 then begin j := starPos + 1; Inc(matchPos); i := matchPos; end
    else begin IsMatch := False; Exit; end;
  end;
  while (j <= Length(p)) and (p[j] = '*') do Inc(j);
  IsMatch := j > Length(p);
end;
var t,tc,n,m: LongInt; s,p: AnsiString;
begin
  if EOF then Halt(0); ReadLn(t);
  for tc := 1 to t do begin
    ReadLn(n); if n > 0 then ReadLn(s) else s := '';
    ReadLn(m); if m > 0 then ReadLn(p) else p := '';
    if tc > 1 then WriteLn;
    if IsMatch(s,p) then Write('true') else Write('false');
  end;
end.
