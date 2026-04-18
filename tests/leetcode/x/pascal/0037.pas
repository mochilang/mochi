program Main;
uses SysUtils;

type
  TBoard = array[0..8, 0..8] of Char;

var
  t, tc: Integer;
  board: TBoard;

function Valid(r, c: Integer; ch: Char): Boolean;
var
  i, j, br, bc: Integer;
begin
  for i := 0 to 8 do
    if (board[r, i] = ch) or (board[i, c] = ch) then
    begin
      Valid := False;
      Exit;
    end;
  br := (r div 3) * 3;
  bc := (c div 3) * 3;
  for i := br to br + 2 do
    for j := bc to bc + 2 do
      if board[i, j] = ch then
      begin
        Valid := False;
        Exit;
      end;
  Valid := True;
end;

function Solve: Boolean;
var
  r, c, d: Integer;
  ch: Char;
begin
  for r := 0 to 8 do
    for c := 0 to 8 do
      if board[r, c] = '.' then
      begin
        for d := 1 to 9 do
        begin
          ch := Chr(Ord('0') + d);
          if Valid(r, c, ch) then
          begin
            board[r, c] := ch;
            if Solve() then
            begin
              Solve := True;
              Exit;
            end;
            board[r, c] := '.';
          end;
        end;
        Solve := False;
        Exit;
      end;
  Solve := True;
end;

procedure ReadBoard;
var
  i, j: Integer;
  line: AnsiString;
begin
  for i := 0 to 8 do
  begin
    ReadLn(line);
    for j := 0 to 8 do
      board[i, j] := line[j + 1];
  end;
end;

procedure WriteBoard;
var
  i, j: Integer;
begin
  for i := 0 to 8 do
  begin
    for j := 0 to 8 do
      Write(board[i, j]);
    if (tc < t) or (i < 8) then
      WriteLn;
  end;
end;

begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 1 to t do
  begin
    ReadBoard;
    Solve();
    WriteBoard;
  end;
end.
