program Main;

uses SysUtils;

function ConvertZigzag(const s: AnsiString; numRows: LongInt): AnsiString;
var
  cycle, row, i, diag: LongInt;
begin
  if (numRows <= 1) or (numRows >= Length(s)) then
  begin
    ConvertZigzag := s;
    Exit;
  end;
  cycle := 2 * numRows - 2;
  ConvertZigzag := '';
  for row := 1 to numRows do
  begin
    i := row;
    while i <= Length(s) do
    begin
      ConvertZigzag := ConvertZigzag + s[i];
      diag := i + cycle - 2 * (row - 1);
      if (row > 1) and (row < numRows) and (diag <= Length(s)) then
        ConvertZigzag := ConvertZigzag + s[diag];
      i := i + cycle;
    end;
  end;
end;

var
  t, caseNo, numRows: LongInt;
  s: AnsiString;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for caseNo := 1 to t do
  begin
    ReadLn(s);
    ReadLn(numRows);
    Write(ConvertZigzag(s, numRows));
    if caseNo < t then WriteLn;
  end;
end.
