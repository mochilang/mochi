program Main;
uses SysUtils;

function MatchAt(const s, p: AnsiString; i, j: Integer): Boolean;
var
  first: Boolean;
begin
  if j > Length(p) then
  begin
    MatchAt := i > Length(s);
    Exit;
  end;
  first := (i <= Length(s)) and ((p[j] = '.') or (s[i] = p[j]));
  if (j + 1 <= Length(p)) and (p[j + 1] = '*') then
    MatchAt := MatchAt(s, p, i, j + 2) or (first and MatchAt(s, p, i + 1, j))
  else
    MatchAt := first and MatchAt(s, p, i + 1, j + 1);
end;

var
  t, tc: Integer;
  s, p: AnsiString;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 1 to t do
  begin
    ReadLn(s);
    ReadLn(p);
    if MatchAt(s, p, 1, 1) then Write('true') else Write('false');
    if tc < t then WriteLn;
  end;
end.
