{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function depth_first_search(depth_first_search_grid: IntArrayArray; depth_first_search_row: int64; depth_first_search_col: int64; depth_first_search_visit: BoolArrayArray): int64; forward;
function count_paths(count_paths_grid: IntArrayArray): int64; forward;
procedure main(); forward;
function depth_first_search(depth_first_search_grid: IntArrayArray; depth_first_search_row: int64; depth_first_search_col: int64; depth_first_search_visit: BoolArrayArray): int64;
var
  depth_first_search_row_length: integer;
  depth_first_search_col_length: integer;
  depth_first_search_count: int64;
begin
  depth_first_search_row_length := Length(depth_first_search_grid);
  depth_first_search_col_length := Length(depth_first_search_grid[0]);
  if (((depth_first_search_row < 0) or (depth_first_search_col < 0)) or (depth_first_search_row = depth_first_search_row_length)) or (depth_first_search_col = depth_first_search_col_length) then begin
  exit(0);
end;
  if depth_first_search_visit[depth_first_search_row][depth_first_search_col] then begin
  exit(0);
end;
  if depth_first_search_grid[depth_first_search_row][depth_first_search_col] = 1 then begin
  exit(0);
end;
  if (depth_first_search_row = (depth_first_search_row_length - 1)) and (depth_first_search_col = (depth_first_search_col_length - 1)) then begin
  exit(1);
end;
  depth_first_search_visit[depth_first_search_row][depth_first_search_col] := true;
  depth_first_search_count := 0;
  depth_first_search_count := depth_first_search_count + depth_first_search(depth_first_search_grid, depth_first_search_row + 1, depth_first_search_col, depth_first_search_visit);
  depth_first_search_count := depth_first_search_count + depth_first_search(depth_first_search_grid, depth_first_search_row - 1, depth_first_search_col, depth_first_search_visit);
  depth_first_search_count := depth_first_search_count + depth_first_search(depth_first_search_grid, depth_first_search_row, depth_first_search_col + 1, depth_first_search_visit);
  depth_first_search_count := depth_first_search_count + depth_first_search(depth_first_search_grid, depth_first_search_row, depth_first_search_col - 1, depth_first_search_visit);
  depth_first_search_visit[depth_first_search_row][depth_first_search_col] := false;
  exit(depth_first_search_count);
end;
function count_paths(count_paths_grid: IntArrayArray): int64;
var
  count_paths_rows: integer;
  count_paths_cols: integer;
  count_paths_visit: array of BoolArray;
  count_paths_i: int64;
  count_paths_row_visit: array of boolean;
  count_paths_j: int64;
begin
  count_paths_rows := Length(count_paths_grid);
  count_paths_cols := Length(count_paths_grid[0]);
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
  exit(depth_first_search(count_paths_grid, 0, 0, count_paths_visit));
end;
procedure main();
var
  main_grid1: array of array of int64;
  main_grid2: array of array of int64;
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
  writeln('');
end.
