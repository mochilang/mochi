{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  board: IntArrayArray;
function get_valid_pos(position: IntArray; n: integer): IntArrayArray; forward;
function is_complete(board: IntArrayArray): boolean; forward;
function open_knight_tour_helper(board: IntArrayArray; pos: IntArray; curr: integer): boolean; forward;
function open_knight_tour(n: integer): IntArrayArray; forward;
function get_valid_pos(position: IntArray; n: integer): IntArrayArray;
var
  get_valid_pos_y: integer;
  get_valid_pos_x: integer;
  get_valid_pos_positions: array of array of integer;
  get_valid_pos_permissible: array of IntArray;
  get_valid_pos_idx: integer;
  get_valid_pos_inner: array of integer;
  get_valid_pos_y_test: integer;
  get_valid_pos_x_test: integer;
begin
  get_valid_pos_y := position[0];
  get_valid_pos_x := position[1];
  get_valid_pos_positions := [[get_valid_pos_y + 1, get_valid_pos_x + 2], [get_valid_pos_y - 1, get_valid_pos_x + 2], [get_valid_pos_y + 1, get_valid_pos_x - 2], [get_valid_pos_y - 1, get_valid_pos_x - 2], [get_valid_pos_y + 2, get_valid_pos_x + 1], [get_valid_pos_y + 2, get_valid_pos_x - 1], [get_valid_pos_y - 2, get_valid_pos_x + 1], [get_valid_pos_y - 2, get_valid_pos_x - 1]];
  get_valid_pos_permissible := [];
  for get_valid_pos_idx := 0 to (Length(get_valid_pos_positions) - 1) do begin
  get_valid_pos_inner := get_valid_pos_positions[get_valid_pos_idx];
  get_valid_pos_y_test := get_valid_pos_inner[0];
  get_valid_pos_x_test := get_valid_pos_inner[1];
  if (((get_valid_pos_y_test >= 0) and (get_valid_pos_y_test < n)) and (get_valid_pos_x_test >= 0)) and (get_valid_pos_x_test < n) then begin
  get_valid_pos_permissible := concat(get_valid_pos_permissible, [get_valid_pos_inner]);
end;
end;
  exit(get_valid_pos_permissible);
end;
function is_complete(board: IntArrayArray): boolean;
var
  is_complete_i: integer;
  is_complete_row: array of integer;
  is_complete_j: integer;
begin
  for is_complete_i := 0 to (Length(board) - 1) do begin
  is_complete_row := board[is_complete_i];
  for is_complete_j := 0 to (Length(is_complete_row) - 1) do begin
  if is_complete_row[is_complete_j] = 0 then begin
  exit(false);
end;
end;
end;
  exit(true);
end;
function open_knight_tour_helper(board: IntArrayArray; pos: IntArray; curr: integer): boolean;
var
  open_knight_tour_helper_moves: IntArrayArray;
  open_knight_tour_helper_i: integer;
  open_knight_tour_helper_position: array of integer;
  open_knight_tour_helper_y: integer;
  open_knight_tour_helper_x: integer;
begin
  if is_complete(board) then begin
  exit(true);
end;
  open_knight_tour_helper_moves := get_valid_pos(pos, Length(board));
  for open_knight_tour_helper_i := 0 to (Length(open_knight_tour_helper_moves) - 1) do begin
  open_knight_tour_helper_position := open_knight_tour_helper_moves[open_knight_tour_helper_i];
  open_knight_tour_helper_y := open_knight_tour_helper_position[0];
  open_knight_tour_helper_x := open_knight_tour_helper_position[1];
  if board[open_knight_tour_helper_y][open_knight_tour_helper_x] = 0 then begin
  board[open_knight_tour_helper_y][open_knight_tour_helper_x] := curr + 1;
  if open_knight_tour_helper(board, open_knight_tour_helper_position, curr + 1) then begin
  exit(true);
end;
  board[open_knight_tour_helper_y][open_knight_tour_helper_x] := 0;
end;
end;
  exit(false);
end;
function open_knight_tour(n: integer): IntArrayArray;
var
  open_knight_tour_board: array of IntArray;
  open_knight_tour_i: integer;
  open_knight_tour_row: array of integer;
  open_knight_tour_j: integer;
begin
  open_knight_tour_board := [];
  for open_knight_tour_i := 0 to (n - 1) do begin
  open_knight_tour_row := [];
  for open_knight_tour_j := 0 to (n - 1) do begin
  open_knight_tour_row := concat(open_knight_tour_row, [0]);
end;
  open_knight_tour_board := concat(open_knight_tour_board, [open_knight_tour_row]);
end;
  for open_knight_tour_i := 0 to (n - 1) do begin
  for open_knight_tour_j := 0 to (n - 1) do begin
  open_knight_tour_board[open_knight_tour_i][open_knight_tour_j] := 1;
  if open_knight_tour_helper(open_knight_tour_board, [open_knight_tour_i, open_knight_tour_j], 1) then begin
  exit(open_knight_tour_board);
end;
  open_knight_tour_board[open_knight_tour_i][open_knight_tour_j] := 0;
end;
end;
  writeln('Open Knight Tour cannot be performed on a board of size ' + IntToStr(n));
  exit(open_knight_tour_board);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  board := open_knight_tour(1);
  writeln(board[0][0]);
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
