{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of integer;
type BoolArray = array of boolean;
type IntArrayArray = array of IntArray;
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  grid: array of IntArray;
function is_safe(is_safe_grid: IntArrayArray; is_safe_visited: BoolArrayArray; is_safe_row: integer; is_safe_col: integer): boolean; forward;
procedure dfs(dfs_grid: IntArrayArray; dfs_visited: BoolArrayArray; dfs_row: integer; dfs_col: integer); forward;
function count_islands(count_islands_grid: IntArrayArray): integer; forward;
function is_safe(is_safe_grid: IntArrayArray; is_safe_visited: BoolArrayArray; is_safe_row: integer; is_safe_col: integer): boolean;
var
  is_safe_rows: integer;
  is_safe_cols: integer;
  is_safe_within_bounds: boolean;
  is_safe_visited_cell: boolean;
  is_safe_not_visited: boolean;
begin
  is_safe_rows := Length(is_safe_grid);
  is_safe_cols := Length(is_safe_grid[0]);
  is_safe_within_bounds := (((is_safe_row >= 0) and (is_safe_row < is_safe_rows)) and (is_safe_col >= 0)) and (is_safe_col < is_safe_cols);
  if not is_safe_within_bounds then begin
  exit(false);
end;
  is_safe_visited_cell := is_safe_visited[is_safe_row][is_safe_col];
  is_safe_not_visited := is_safe_visited_cell = false;
  exit(is_safe_not_visited and (is_safe_grid[is_safe_row][is_safe_col] = 1));
end;
procedure dfs(dfs_grid: IntArrayArray; dfs_visited: BoolArrayArray; dfs_row: integer; dfs_col: integer);
var
  dfs_row_nbr: array of integer;
  dfs_col_nbr: array of integer;
  dfs_k: integer;
  dfs_new_row: integer;
  dfs_new_col: integer;
begin
  dfs_row_nbr := [-1, -1, -1, 0, 0, 1, 1, 1];
  dfs_col_nbr := [-1, 0, 1, -1, 1, -1, 0, 1];
  dfs_visited[dfs_row][dfs_col] := true;
  dfs_k := 0;
  while dfs_k < 8 do begin
  dfs_new_row := dfs_row + dfs_row_nbr[dfs_k];
  dfs_new_col := dfs_col + dfs_col_nbr[dfs_k];
  if is_safe(dfs_grid, dfs_visited, dfs_new_row, dfs_new_col) then begin
  dfs(dfs_grid, dfs_visited, dfs_new_row, dfs_new_col);
end;
  dfs_k := dfs_k + 1;
end;
end;
function count_islands(count_islands_grid: IntArrayArray): integer;
var
  count_islands_rows: integer;
  count_islands_cols: integer;
  count_islands_visited: array of BoolArray;
  count_islands_i: integer;
  count_islands_row_list: array of boolean;
  count_islands_j: integer;
  count_islands_count: integer;
begin
  count_islands_rows := Length(count_islands_grid);
  count_islands_cols := Length(count_islands_grid[0]);
  count_islands_visited := [];
  count_islands_i := 0;
  while count_islands_i < count_islands_rows do begin
  count_islands_row_list := [];
  count_islands_j := 0;
  while count_islands_j < count_islands_cols do begin
  count_islands_row_list := concat(count_islands_row_list, [false]);
  count_islands_j := count_islands_j + 1;
end;
  count_islands_visited := concat(count_islands_visited, [count_islands_row_list]);
  count_islands_i := count_islands_i + 1;
end;
  count_islands_count := 0;
  count_islands_i := 0;
  while count_islands_i < count_islands_rows do begin
  count_islands_j := 0;
  while count_islands_j < count_islands_cols do begin
  if not count_islands_visited[count_islands_i][count_islands_j] and (count_islands_grid[count_islands_i][count_islands_j] = 1) then begin
  dfs(count_islands_grid, count_islands_visited, count_islands_i, count_islands_j);
  count_islands_count := count_islands_count + 1;
end;
  count_islands_j := count_islands_j + 1;
end;
  count_islands_i := count_islands_i + 1;
end;
  exit(count_islands_count);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  grid := [[1, 1, 0, 0, 0], [0, 1, 0, 0, 1], [1, 0, 0, 1, 1], [0, 0, 0, 0, 0], [1, 0, 1, 0, 1]];
  writeln(count_islands(grid));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
