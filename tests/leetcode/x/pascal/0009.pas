program PalindromeNumber;

var
  t, i, x, n, original: LongInt;
  rev: Int64;

begin
  if EOF then Halt(0);
  Read(t);
  for i := 1 to t do
  begin
    Read(x);
    if x < 0 then
      WriteLn('false')
    else
    begin
      original := x;
      n := x;
      rev := 0;
      while n > 0 do
      begin
        rev := rev * 10 + (n mod 10);
        n := n div 10;
      end;
      if rev = original then WriteLn('true') else WriteLn('false');
    end;
  end;
end.
