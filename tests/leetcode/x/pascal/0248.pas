program Main;

uses SysUtils;

const
  PairCount = 5;
  PairA: array[1..PairCount] of Char = ('0', '1', '6', '8', '9');
  PairB: array[1..PairCount] of Char = ('0', '1', '9', '8', '6');

function CountBuild(var buf: string; leftPos, rightPos, len: LongInt; low, high: string): LongInt;
var
  i: LongInt;
begin
  if leftPos > rightPos then
  begin
    if ((len = Length(low)) and (buf < low)) or ((len = Length(high)) and (buf > high)) then
      Exit(0);
    Exit(1);
  end;
  CountBuild := 0;
  for i := 1 to PairCount do
  begin
    if (leftPos = 1) and (len > 1) and (PairA[i] = '0') then Continue;
    if (leftPos = rightPos) and (PairA[i] <> PairB[i]) then Continue;
    buf[leftPos] := PairA[i];
    buf[rightPos] := PairB[i];
    CountBuild := CountBuild + CountBuild(buf, leftPos + 1, rightPos - 1, len, low, high);
  end;
end;

function CountRange(low, high: string): LongInt;
var
  len: LongInt;
  buf: string;
begin
  CountRange := 0;
  for len := Length(low) to Length(high) do
  begin
    SetLength(buf, len);
    CountRange := CountRange + CountBuild(buf, 1, len, len, low, high);
  end;
end;

var
  t, i: LongInt;
  low, high: string;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for i := 1 to t do
  begin
    ReadLn(low);
    ReadLn(high);
    if i > 1 then WriteLn;
    Write(CountRange(low, high));
  end;
end.
