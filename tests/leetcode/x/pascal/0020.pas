program ValidParentheses;

function Matches(closeCh, openCh: Char): Boolean;
begin
  Matches := ((closeCh = ')') and (openCh = '(')) or
             ((closeCh = ']') and (openCh = '[')) or
             ((closeCh = '}') and (openCh = '{'));
end;

function IsValid(s: String): Boolean;
var
  stack: array[1..10005] of Char;
  top, i: LongInt;
  ch, openCh: Char;
begin
  top := 0;
  for i := 1 to Length(s) do
  begin
    ch := s[i];
    if (ch = '(') or (ch = '[') or (ch = '{') then
    begin
      Inc(top);
      stack[top] := ch;
    end
    else
    begin
      if top = 0 then
      begin
        IsValid := False;
        Exit;
      end;
      openCh := stack[top];
      Dec(top);
      if not Matches(ch, openCh) then
      begin
        IsValid := False;
        Exit;
      end;
    end;
  end;
  IsValid := top = 0;
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
    if IsValid(s) then WriteLn('true') else WriteLn('false');
  end;
end.
