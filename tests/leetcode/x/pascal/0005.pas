program Main;

uses SysUtils;

type
  TPair = record
    StartIndex: LongInt;
    Len: LongInt;
  end;

function Expand(const s: AnsiString; left0, right0: LongInt): TPair;
var
  left, right: LongInt;
begin
  left := left0;
  right := right0;
  while (left >= 1) and (right <= Length(s)) and (s[left] = s[right]) do
  begin
    Dec(left);
    Inc(right);
  end;
  Expand.StartIndex := left + 1;
  Expand.Len := right - left - 1;
end;

function LongestPalindrome(const s: AnsiString): AnsiString;
var
  i, bestStart, bestLen: LongInt;
  p: TPair;
begin
  bestStart := 1;
  if Length(s) > 0 then bestLen := 1 else bestLen := 0;
  for i := 1 to Length(s) do
  begin
    p := Expand(s, i, i);
    if p.Len > bestLen then
    begin
      bestStart := p.StartIndex;
      bestLen := p.Len;
    end;
    p := Expand(s, i, i + 1);
    if p.Len > bestLen then
    begin
      bestStart := p.StartIndex;
      bestLen := p.Len;
    end;
  end;
  LongestPalindrome := Copy(s, bestStart, bestLen);
end;

var
  t, i: LongInt;
  s: AnsiString;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for i := 1 to t do
  begin
    ReadLn(s);
    Write(LongestPalindrome(s));
    if i < t then WriteLn;
  end;
end.
