program RomanToInteger;

function ValueOf(c: Char): LongInt;
begin
  case c of
    'I': ValueOf := 1;
    'V': ValueOf := 5;
    'X': ValueOf := 10;
    'L': ValueOf := 50;
    'C': ValueOf := 100;
    'D': ValueOf := 500;
    else ValueOf := 1000;
  end;
end;

function RomanToInt(s: String): LongInt;
var
  i, cur, nxt, total: LongInt;
begin
  total := 0;
  for i := 1 to Length(s) do
  begin
    cur := ValueOf(s[i]);
    if i < Length(s) then nxt := ValueOf(s[i + 1]) else nxt := 0;
    if cur < nxt then total := total - cur else total := total + cur;
  end;
  RomanToInt := total;
end;

var
  t, i: LongInt;
  s: String;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for i := 1 to t do
  begin
    ReadLn(s);
    WriteLn(RomanToInt(s));
  end;
end.
