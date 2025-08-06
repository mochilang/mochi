{$mode objfpc}
program Main;
uses SysUtils;
type BoolArray = array of boolean;
type IntArray = array of integer;
type BoolArrayArray = array of BoolArray;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  direction: integer;
  height: integer;
  y: integer;
  x: integer;
  board: BoolArrayArray;
  width: integer;
  steps: integer;
function create_board(width: integer; height: integer): BoolArrayArray; forward;
function move_ant(board: BoolArrayArray; x: integer; y: integer; direction: integer): IntArray; forward;
function langtons_ant(width: integer; height: integer; steps: integer): BoolArrayArray; forward;
function create_board(width: integer; height: integer): BoolArrayArray;
var
  create_board_board: array of BoolArray;
  create_board_i: integer;
  create_board_row: array of boolean;
  create_board_j: integer;
begin
  create_board_board := [];
  create_board_i := 0;
  while create_board_i < height do begin
  create_board_row := [];
  create_board_j := 0;
  while create_board_j < width do begin
  create_board_row := concat(create_board_row, [true]);
  create_board_j := create_board_j + 1;
end;
  create_board_board := concat(create_board_board, [create_board_row]);
  create_board_i := create_board_i + 1;
end;
  exit(create_board_board);
end;
function move_ant(board: BoolArrayArray; x: integer; y: integer; direction: integer): IntArray;
var
  move_ant_old_x: integer;
  move_ant_old_y: integer;
begin
  if board[x][y] then begin
  direction := (direction + 1) mod 4;
end else begin
  direction := (direction + 3) mod 4;
end;
  move_ant_old_x := x;
  move_ant_old_y := y;
  if direction = 0 then begin
  x := x - 1;
end else begin
  if direction = 1 then begin
  y := y + 1;
end else begin
  if direction = 2 then begin
  x := x + 1;
end else begin
  y := y - 1;
end;
end;
end;
  board[move_ant_old_x][move_ant_old_y] := not board[move_ant_old_x][move_ant_old_y];
  exit([x, y, direction]);
end;
function langtons_ant(width: integer; height: integer; steps: integer): BoolArrayArray;
var
  langtons_ant_board: BoolArrayArray;
  langtons_ant_x: integer;
  langtons_ant_y: integer;
  langtons_ant_dir: integer;
  langtons_ant_s: integer;
  langtons_ant_state: IntArray;
begin
  langtons_ant_board := create_board(width, height);
  langtons_ant_x := width div 2;
  langtons_ant_y := height div 2;
  langtons_ant_dir := 3;
  langtons_ant_s := 0;
  while langtons_ant_s < steps do begin
  langtons_ant_state := move_ant(langtons_ant_board, langtons_ant_x, langtons_ant_y, langtons_ant_dir);
  langtons_ant_x := langtons_ant_state[0];
  langtons_ant_y := langtons_ant_state[1];
  langtons_ant_dir := langtons_ant_state[2];
  langtons_ant_s := langtons_ant_s + 1;
end;
  exit(langtons_ant_board);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
