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
  m1: array of IntArray;
  m2: array of IntArray;
  matrix: IntArrayArray;
  a: integer;
  b: integer;
function min_int(a: integer; b: integer): integer; forward;
function minimum_cost_path(matrix: IntArrayArray): integer; forward;
function min_int(a: integer; b: integer): integer;
begin
  if a < b then begin
  exit(a);
end;
  exit(b);
end;
function minimum_cost_path(matrix: IntArrayArray): integer;
var
  minimum_cost_path_rows: integer;
  minimum_cost_path_cols: integer;
  minimum_cost_path_j: integer;
  minimum_cost_path_row0: array of integer;
  minimum_cost_path_i: integer;
  minimum_cost_path_row: array of integer;
  minimum_cost_path_up: integer;
  minimum_cost_path_left: integer;
  minimum_cost_path_best: integer;
begin
  minimum_cost_path_rows := Length(matrix);
  minimum_cost_path_cols := Length(matrix[0]);
  minimum_cost_path_j := 1;
  while minimum_cost_path_j < minimum_cost_path_cols do begin
  minimum_cost_path_row0 := matrix[0];
  minimum_cost_path_row0[minimum_cost_path_j] := minimum_cost_path_row0[minimum_cost_path_j] + minimum_cost_path_row0[minimum_cost_path_j - 1];
  matrix[0] := minimum_cost_path_row0;
  minimum_cost_path_j := minimum_cost_path_j + 1;
end;
  minimum_cost_path_i := 1;
  while minimum_cost_path_i < minimum_cost_path_rows do begin
  minimum_cost_path_row := matrix[minimum_cost_path_i];
  minimum_cost_path_row[0] := minimum_cost_path_row[0] + matrix[minimum_cost_path_i - 1][0];
  matrix[minimum_cost_path_i] := minimum_cost_path_row;
  minimum_cost_path_i := minimum_cost_path_i + 1;
end;
  minimum_cost_path_i := 1;
  while minimum_cost_path_i < minimum_cost_path_rows do begin
  minimum_cost_path_row := matrix[minimum_cost_path_i];
  minimum_cost_path_j := 1;
  while minimum_cost_path_j < minimum_cost_path_cols do begin
  minimum_cost_path_up := matrix[minimum_cost_path_i - 1][minimum_cost_path_j];
  minimum_cost_path_left := minimum_cost_path_row[minimum_cost_path_j - 1];
  minimum_cost_path_best := min_int(minimum_cost_path_up, minimum_cost_path_left);
  minimum_cost_path_row[minimum_cost_path_j] := minimum_cost_path_row[minimum_cost_path_j] + minimum_cost_path_best;
  minimum_cost_path_j := minimum_cost_path_j + 1;
end;
  matrix[minimum_cost_path_i] := minimum_cost_path_row;
  minimum_cost_path_i := minimum_cost_path_i + 1;
end;
  exit(matrix[minimum_cost_path_rows - 1][minimum_cost_path_cols - 1]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  m1 := [[2, 1], [3, 1], [4, 2]];
  m2 := [[2, 1, 4], [2, 1, 3], [3, 2, 1]];
  writeln(IntToStr(minimum_cost_path(m1)));
  writeln(IntToStr(minimum_cost_path(m2)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
