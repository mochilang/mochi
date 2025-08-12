{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type StrArray = array of string;
type StrArrayArray = array of StrArray;
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
    _now := Integer(GetTickCount64()*1000);
  end;
end;
function _bench_now(): int64;
begin
  _bench_now := GetTickCount64()*1000;
end;
function _mem(): int64;
var h: TFPCHeapStatus;
begin
  h := GetFPCHeapStatus;
  _mem := h.CurrHeapUsed;
end;
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
procedure error(msg: string);
begin
  panic(msg);
end;
function contains(xs: array of integer; v: integer): boolean;
var i: integer;
begin
  for i := 0 to High(xs) do begin
    if xs[i] = v then begin
      contains := true; exit;
    end;
  end;
  contains := false;
end;
function contains(xs: array of string; v: string): boolean;
var i: integer;
begin
  for i := 0 to High(xs) do begin
    if xs[i] = v then begin
      contains := true; exit;
    end;
  end;
  contains := false;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  NUM_SQUARES: integer;
  EMPTY_CELL: string;
  valid_board: array of StrArray;
  invalid_board: array of StrArray;
  board: StrArrayArray;
function is_valid_sudoku_board(board: StrArrayArray): boolean; forward;
function is_valid_sudoku_board(board: StrArrayArray): boolean;
var
  is_valid_sudoku_board_i: integer;
  is_valid_sudoku_board_rows: array of StrArray;
  is_valid_sudoku_board_cols: array of StrArray;
  is_valid_sudoku_board_boxes: array of StrArray;
  is_valid_sudoku_board_r: int64;
  is_valid_sudoku_board_c: int64;
  is_valid_sudoku_board_value: string;
  is_valid_sudoku_board_box: integer;
begin
  if Length(board) <> NUM_SQUARES then begin
  exit(false);
end;
  is_valid_sudoku_board_i := 0;
  while is_valid_sudoku_board_i < NUM_SQUARES do begin
  if Length(board[is_valid_sudoku_board_i]) <> NUM_SQUARES then begin
  exit(false);
end;
  is_valid_sudoku_board_i := is_valid_sudoku_board_i + 1;
end;
  is_valid_sudoku_board_rows := [];
  is_valid_sudoku_board_cols := [];
  is_valid_sudoku_board_boxes := [];
  is_valid_sudoku_board_i := 0;
  while is_valid_sudoku_board_i < NUM_SQUARES do begin
  is_valid_sudoku_board_rows := concat(is_valid_sudoku_board_rows, [[]]);
  is_valid_sudoku_board_cols := concat(is_valid_sudoku_board_cols, [[]]);
  is_valid_sudoku_board_boxes := concat(is_valid_sudoku_board_boxes, [[]]);
  is_valid_sudoku_board_i := is_valid_sudoku_board_i + 1;
end;
  for is_valid_sudoku_board_r := 0 to (NUM_SQUARES - 1) do begin
  for is_valid_sudoku_board_c := 0 to (NUM_SQUARES - 1) do begin
  is_valid_sudoku_board_value := board[is_valid_sudoku_board_r][is_valid_sudoku_board_c];
  if is_valid_sudoku_board_value = EMPTY_CELL then begin
  continue;
end;
  is_valid_sudoku_board_box := (Trunc(is_valid_sudoku_board_r div 3) * 3) + Trunc(is_valid_sudoku_board_c div 3);
  if (contains(is_valid_sudoku_board_rows[is_valid_sudoku_board_r], is_valid_sudoku_board_value) or contains(is_valid_sudoku_board_cols[is_valid_sudoku_board_c], is_valid_sudoku_board_value)) or contains(is_valid_sudoku_board_boxes[is_valid_sudoku_board_box], is_valid_sudoku_board_value) then begin
  exit(false);
end;
  is_valid_sudoku_board_rows[is_valid_sudoku_board_r] := concat(is_valid_sudoku_board_rows[is_valid_sudoku_board_r], StrArray([is_valid_sudoku_board_value]));
  is_valid_sudoku_board_cols[is_valid_sudoku_board_c] := concat(is_valid_sudoku_board_cols[is_valid_sudoku_board_c], StrArray([is_valid_sudoku_board_value]));
  is_valid_sudoku_board_boxes[is_valid_sudoku_board_box] := concat(is_valid_sudoku_board_boxes[is_valid_sudoku_board_box], StrArray([is_valid_sudoku_board_value]));
end;
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  NUM_SQUARES := 9;
  EMPTY_CELL := '.';
  valid_board := [['5', '3', '.', '.', '7', '.', '.', '.', '.'], ['6', '.', '.', '1', '9', '5', '.', '.', '.'], ['.', '9', '8', '.', '.', '.', '.', '6', '.'], ['8', '.', '.', '.', '6', '.', '.', '.', '3'], ['4', '.', '.', '8', '.', '3', '.', '.', '1'], ['7', '.', '.', '.', '2', '.', '.', '.', '6'], ['.', '6', '.', '.', '.', '.', '2', '8', '.'], ['.', '.', '.', '4', '1', '9', '.', '.', '5'], ['.', '.', '.', '.', '8', '.', '.', '7', '9']];
  invalid_board := [['8', '3', '.', '.', '7', '.', '.', '.', '.'], ['6', '.', '.', '1', '9', '5', '.', '.', '.'], ['.', '9', '8', '.', '.', '.', '.', '6', '.'], ['8', '.', '.', '.', '6', '.', '.', '.', '3'], ['4', '.', '.', '8', '.', '3', '.', '.', '1'], ['7', '.', '.', '.', '2', '.', '.', '.', '6'], ['.', '6', '.', '.', '.', '.', '2', '8', '.'], ['.', '.', '.', '4', '1', '9', '.', '.', '5'], ['.', '.', '.', '.', '8', '.', '.', '7', '9']];
  writeln(Ord(is_valid_sudoku_board(valid_board)));
  writeln(Ord(is_valid_sudoku_board(invalid_board)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
