program Main;

uses SysUtils;

function Calculate(expr: string): LongInt;
var
  i, resultValue, number, sign, prevSign, prevResult: LongInt;
  stack: array of LongInt;
begin
  resultValue := 0;
  number := 0;
  sign := 1;
  SetLength(stack, 0);
  for i := 1 to Length(expr) do
  begin
    if expr[i] in ['0'..'9'] then
      number := number * 10 + Ord(expr[i]) - Ord('0')
    else if (expr[i] = '+') or (expr[i] = '-') then
    begin
      resultValue := resultValue + sign * number;
      number := 0;
      if expr[i] = '+' then sign := 1 else sign := -1;
    end
    else if expr[i] = '(' then
    begin
      SetLength(stack, Length(stack) + 2);
      stack[High(stack) - 1] := resultValue;
      stack[High(stack)] := sign;
      resultValue := 0;
      number := 0;
      sign := 1;
    end
    else if expr[i] = ')' then
    begin
      resultValue := resultValue + sign * number;
      number := 0;
      prevSign := stack[High(stack)];
      prevResult := stack[High(stack) - 1];
      SetLength(stack, Length(stack) - 2);
      resultValue := prevResult + prevSign * resultValue;
    end;
  end;
  Calculate := resultValue + sign * number;
end;

var
  t, i: LongInt;
  line: string;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for i := 1 to t do
  begin
    ReadLn(line);
    if i > 1 then WriteLn;
    Write(Calculate(line));
  end;
end.
