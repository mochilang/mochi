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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  row: integer;
  grid: IntArrayArray;
  visit: BoolArrayArray;
  col: integer;
function depth_first_search(grid: IntArrayArray; row: integer; col: integer; visit: BoolArrayArray): integer; forward;
function count_paths(grid: IntArrayArray): integer; forward;
procedure main(); forward;
function depth_first_search(grid: IntArrayArray; row: integer; col: integer; visit: BoolArrayArray): integer;
var
  depth_first_search_row_length: integer;
  depth_first_search_col_length: integer;
  depth_first_search_count: integer;
begin
  depth_first_search_row_length := Length(grid);
  depth_first_search_col_length := Length(grid[0]);
  if (((row < 0) or (col < 0)) or (row = depth_first_search_row_length)) or (col = depth_first_search_col_length) then begin
  exit(0);
end;
  if visit[row][col] then begin
  exit(0);
end;
  if grid[row][col] = 1 then begin
  exit(0);
end;
  if (row = (depth_first_search_row_length - 1)) and (col = (depth_first_search_col_length - 1)) then begin
  exit(1);
end;
  visit[row][col] := true;
  depth_first_search_count := 0;
  depth_first_search_count := depth_first_search_count + depth_first_search(grid, row + 1, col, visit);
  depth_first_search_count := depth_first_search_count + depth_first_search(grid, row - 1, col, visit);
  depth_first_search_count := depth_first_search_count + depth_first_search(grid, row, col + 1, visit);
  depth_first_search_count := depth_first_search_count + depth_first_search(grid, row, col - 1, visit);
  visit[row][col] := false;
  exit(depth_first_search_count);
end;
function count_paths(grid: IntArrayArray): integer;
var
  count_paths_rows: integer;
  count_paths_cols: integer;
  count_paths_visit: array of BoolArray;
  count_paths_i: integer;
  count_paths_row_visit: array of boolean;
  count_paths_j: integer;
begin
  count_paths_rows := Length(grid);
  count_paths_cols := Length(grid[0]);
  count_paths_visit := [];
  count_paths_i := 0;
  while count_paths_i < count_paths_rows do begin
  count_paths_row_visit := [];
  count_paths_j := 0;
  while count_paths_j < count_paths_cols do begin
  count_paths_row_visit := concat(count_paths_row_visit, [false]);
  count_paths_j := count_paths_j + 1;
end;
  count_paths_visit := concat(count_paths_visit, [count_paths_row_visit]);
  count_paths_i := count_paths_i + 1;
end;
  exit(depth_first_search(grid, 0, 0, count_paths_visit));
end;
procedure main();
var
  main_grid1: array of array of integer;
  main_grid2: array of array of integer;
begin
  main_grid1 := [[0, 0, 0, 0], [1, 1, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0]];
  writeln(IntToStr(count_paths(main_grid1)));
  main_grid2 := [[0, 0, 0, 0, 0], [0, 1, 1, 1, 0], [0, 1, 1, 1, 0], [0, 0, 0, 0, 0]];
  writeln(IntToStr(count_paths(main_grid2)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
