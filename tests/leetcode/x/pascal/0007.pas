program Main;

uses SysUtils;

function ReverseInt(x: LongInt): LongInt;
var
  ans, digit: LongInt;
begin
  ans := 0;
  while x <> 0 do
  begin
    digit := x mod 10;
    x := x div 10;
    if (ans > High(LongInt) div 10) or ((ans = High(LongInt) div 10) and (digit > 7)) then
    begin
      ReverseInt := 0;
      Exit;
    end;
    if (ans < Low(LongInt) div 10) or ((ans = Low(LongInt) div 10) and (digit < -8)) then
    begin
      ReverseInt := 0;
      Exit;
    end;
    ans := ans * 10 + digit;
  end;
  ReverseInt := ans;
end;

var
  t, i, x: LongInt;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for i := 1 to t do
  begin
    ReadLn(x);
    Write(ReverseInt(x));
    if i < t then WriteLn;
  end;
end.
