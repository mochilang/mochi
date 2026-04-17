program Main;

uses SysUtils;

function MyAtoi(const s: AnsiString): LongInt;
var
  i, sign, ans, digit, limit: LongInt;
begin
  i := 1;
  while (i <= Length(s)) and (s[i] = ' ') do Inc(i);
  sign := 1;
  if (i <= Length(s)) and ((s[i] = '+') or (s[i] = '-')) then
  begin
    if s[i] = '-' then sign := -1;
    Inc(i);
  end;
  ans := 0;
  if sign > 0 then limit := 7 else limit := 8;
  while (i <= Length(s)) and (s[i] >= '0') and (s[i] <= '9') do
  begin
    digit := Ord(s[i]) - Ord('0');
    if (ans > 214748364) or ((ans = 214748364) and (digit > limit)) then
    begin
      if sign > 0 then MyAtoi := High(LongInt) else MyAtoi := Low(LongInt);
      Exit;
    end;
    ans := ans * 10 + digit;
    Inc(i);
  end;
  MyAtoi := sign * ans;
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
    Write(MyAtoi(s));
    if i < t then WriteLn;
  end;
end.
