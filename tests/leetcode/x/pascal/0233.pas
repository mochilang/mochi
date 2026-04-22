program Main;

uses SysUtils;

function CountDigitOne(n: Int64): Int64;
var
  total, m, high, cur, low: Int64;
begin
  total := 0;
  m := 1;
  while m <= n do
  begin
    high := n div (m * 10);
    cur := (n div m) mod 10;
    low := n mod m;
    if cur = 0 then
      total := total + high * m
    else if cur = 1 then
      total := total + high * m + low + 1
    else
      total := total + (high + 1) * m;
    m := m * 10;
  end;
  CountDigitOne := total;
end;

var
  t, i: LongInt;
  n: Int64;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for i := 1 to t do
  begin
    ReadLn(n);
    if i > 1 then WriteLn;
    Write(CountDigitOne(n));
  end;
end.
