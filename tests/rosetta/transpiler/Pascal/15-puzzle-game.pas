{$mode objfpc}
program Main;
uses SysUtils;
var _nowSeed: int64 = 0;
var _nowSeeded: boolean = false;
procedure init_now();
var s: string; v: int64;
begin
  s := GetEnvironmentVariable('MOCHI_NOW_SEED');
  if s <> '' then begin
    Val(s, v);
    _nowSeed := v;
    _nowSeeded := true;
  end;
end;
function _now(): integer;
begin
  if _nowSeeded then begin
    _nowSeed := (_nowSeed * 1664525 + 1013904223) mod 2147483647;
    _now := _nowSeed;
  end else begin
    _now := Integer(GetTickCount64());
  end;
end;
function _input(): string;
var s: string;
begin
  if EOF(Input) then s := '' else ReadLn(s);
  _input := s;
end;
type MoveResult = record
  idx: integer;
  ok: boolean;
end;
var
  board: array of integer;
  solved: array of integer;
  empty: integer;
  moves: integer;
  quit: boolean;
  isSolved_i: integer;
  doMove_r: MoveResult;
  doMove_i: integer;
  doMove_j: integer;
  doMove_tmp: integer;
  shuffle_i: integer;
  printBoard_line: string;
  printBoard_i: integer;
  printBoard_val: integer;
  printBoard_s: string;
  playOneMove_s: string;
  playOneMove_c: string;
  playOneMove_m: integer;
function makeMoveResult(idx: integer; ok: boolean): MoveResult;
begin
  Result.idx := idx;
  Result.ok := ok;
end;
function randMove(): integer;
begin
  exit(_now() mod 4);
end;
function isSolved(): boolean;
begin
  isSolved_i := 0;
  while isSolved_i < 16 do begin
  if board[isSolved_i] <> solved[isSolved_i] then begin
  exit(false);
end;
  isSolved_i := isSolved_i + 1;
end;
  exit(true);
end;
function isValidMove(m: integer): MoveResult;
begin
  if m = 0 then begin
  exit(makeMoveResult(empty - 4, (empty div 4) > 0));
end;
  if m = 1 then begin
  exit(makeMoveResult(empty + 4, (empty div 4) < 3));
end;
  if m = 2 then begin
  exit(makeMoveResult(empty + 1, (empty mod 4) < 3));
end;
  if m = 3 then begin
  exit(makeMoveResult(empty - 1, (empty mod 4) > 0));
end;
  exit(makeMoveResult(0, false));
end;
function doMove(m: integer): boolean;
begin
  doMove_r := isValidMove(m);
  if not doMove_r.ok then begin
  exit(false);
end;
  doMove_i := empty;
  doMove_j := doMove_r.idx;
  doMove_tmp := board[doMove_i];
  board[doMove_i] := board[doMove_j];
  board[doMove_j] := doMove_tmp;
  empty := doMove_j;
  moves := moves + 1;
  exit(true);
end;
procedure shuffle(n: integer);
begin
  shuffle_i := 0;
  while (shuffle_i < n) or isSolved() do begin
  if doMove(randMove()) then begin
  shuffle_i := shuffle_i + 1;
end;
end;
end;
procedure printBoard();
begin
  printBoard_line := '';
  printBoard_i := 0;
  while printBoard_i < 16 do begin
  printBoard_val := board[printBoard_i];
  if printBoard_val = 0 then begin
  printBoard_line := printBoard_line + '  .';
end else begin
  printBoard_s := IntToStr(printBoard_val);
  if printBoard_val < 10 then begin
  printBoard_line := (printBoard_line + '  ') + printBoard_s;
end else begin
  printBoard_line := (printBoard_line + ' ') + printBoard_s;
end;
end;
  if (printBoard_i mod 4) = 3 then begin
  writeln(printBoard_line);
  printBoard_line := '';
end;
  printBoard_i := printBoard_i + 1;
end;
end;
procedure playOneMove();
begin
  while true do begin
  writeln(('Enter move #' + IntToStr(moves + 1)) + ' (U, D, L, R, or Q): ');
  playOneMove_s := _input();
  if playOneMove_s = '' then begin
  continue;
end;
  playOneMove_c := copy(playOneMove_s, 0+1, (1 - (0)));
  playOneMove_m := 0;
  if (playOneMove_c = 'U') or (playOneMove_c = 'u') then begin
  playOneMove_m := 0;
end;
  if not doMove(playOneMove_m) then begin
  writeln('That is not a valid move at the moment.');
  continue;
end;
  exit();
end;
end;
procedure play();
begin
  writeln('Starting board:');
  while not quit and (isSolved() = false) do begin
  writeln('');
  printBoard();
  playOneMove();
end;
  if isSolved() then begin
  writeln(('You solved the puzzle in ' + IntToStr(moves)) + ' moves.');
end;
end;
procedure main();
begin
  shuffle(50);
  play();
end;
begin
  init_now();
  board := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0];
  solved := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0];
  empty := 15;
  moves := 0;
  quit := false;
  main();
end.
