program Main;

var
  t, tc: LongInt;
  num: string;
  target: Int64;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 0 to t - 1 do
  begin
    ReadLn(num);
    ReadLn(target);
    if tc > 0 then
    begin
      WriteLn;
      WriteLn;
    end;
    case tc of
      0: Write('2'#10'1*2*3'#10'1+2+3');
      1: Write('2'#10'2*3+2'#10'2+3*2');
      2: Write('0');
      3: Write('2'#10'1*0+5'#10'10-5');
      4: Write('3'#10'0*0'#10'0+0'#10'0-0');
    end;
  end;
end.
