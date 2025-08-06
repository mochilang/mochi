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
function create_board(n: integer): IntArrayArray; forward;
function is_safe(board: IntArrayArray; row: integer; column: integer): boolean; forward;
function row_string(row: IntArray): string; forward;
procedure printboard(board: IntArrayArray); forward;
function solve(board: IntArrayArray; row: integer): integer; forward;
function n_queens(n: integer): integer; forward;
function create_board(n: integer): IntArrayArray;
var
  create_board_board: array of IntArray;
  create_board_i: integer;
  create_board_row: array of integer;
  create_board_j: integer;
begin
  create_board_board := [];
  create_board_i := 0;
  while create_board_i < n do begin
  create_board_row := [];
  create_board_j := 0;
  while create_board_j < n do begin
  create_board_row := concat(create_board_row, [0]);
  create_board_j := create_board_j + 1;
end;
  create_board_board := concat(create_board_board, [create_board_row]);
  create_board_i := create_board_i + 1;
end;
  exit(create_board_board);
end;
function is_safe(board: IntArrayArray; row: integer; column: integer): boolean;
var
  is_safe_n: integer;
  is_safe_i: integer;
  is_safe_j: integer;
begin
  is_safe_n := Length(board);
  is_safe_i := 0;
  while is_safe_i < row do begin
  if board[is_safe_i][column] = 1 then begin
  exit(false);
end;
  is_safe_i := is_safe_i + 1;
end;
  is_safe_i := row - 1;
  is_safe_j := column - 1;
  while (is_safe_i >= 0) and (is_safe_j >= 0) do begin
  if board[is_safe_i][is_safe_j] = 1 then begin
  exit(false);
end;
  is_safe_i := is_safe_i - 1;
  is_safe_j := is_safe_j - 1;
end;
  is_safe_i := row - 1;
  is_safe_j := column + 1;
  while (is_safe_i >= 0) and (is_safe_j < is_safe_n) do begin
  if board[is_safe_i][is_safe_j] = 1 then begin
  exit(false);
end;
  is_safe_i := is_safe_i - 1;
  is_safe_j := is_safe_j + 1;
end;
  exit(true);
end;
function row_string(row: IntArray): string;
var
  row_string_s: string;
  row_string_j: integer;
begin
  row_string_s := '';
  row_string_j := 0;
  while row_string_j < Length(row) do begin
  if row[row_string_j] = 1 then begin
  row_string_s := row_string_s + 'Q ';
end else begin
  row_string_s := row_string_s + '. ';
end;
  row_string_j := row_string_j + 1;
end;
  exit(row_string_s);
end;
procedure printboard(board: IntArrayArray);
var
  printboard_i: integer;
begin
  printboard_i := 0;
  while printboard_i < Length(board) do begin
  writeln(row_string(board[printboard_i]));
  printboard_i := printboard_i + 1;
end;
end;
function solve(board: IntArrayArray; row: integer): integer;
var
  solve_count: integer;
  solve_i: integer;
begin
  if row >= Length(board) then begin
  printboard(board);
  writeln('');
  exit(1);
end;
  solve_count := 0;
  solve_i := 0;
  while solve_i < Length(board) do begin
  if is_safe(board, row, solve_i) then begin
  board[row][solve_i] := 1;
  solve_count := solve_count + solve(board, row + 1);
  board[row][solve_i] := 0;
end;
  solve_i := solve_i + 1;
end;
  exit(solve_count);
end;
function n_queens(n: integer): integer;
var
  n_queens_board: IntArrayArray;
  n_queens_total: integer;
begin
  n_queens_board := create_board(n);
  n_queens_total := solve(n_queens_board, 0);
  writeln('The total number of solutions are: ' + IntToStr(n_queens_total));
  exit(n_queens_total);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  n_queens(4);
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
