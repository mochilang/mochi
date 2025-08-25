{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  m1: array of IntArray;
  m2: array of IntArray;
function min_int(min_int_a: int64; min_int_b: int64): int64; forward;
function minimum_cost_path(minimum_cost_path_matrix: IntArrayArray): int64; forward;
function min_int(min_int_a: int64; min_int_b: int64): int64;
begin
  if min_int_a < min_int_b then begin
  exit(min_int_a);
end;
  exit(min_int_b);
end;
function minimum_cost_path(minimum_cost_path_matrix: IntArrayArray): int64;
var
  minimum_cost_path_rows: integer;
  minimum_cost_path_cols: integer;
  minimum_cost_path_j: int64;
  minimum_cost_path_row0: array of int64;
  minimum_cost_path_i: int64;
  minimum_cost_path_row: array of int64;
  minimum_cost_path_up: int64;
  minimum_cost_path_left: int64;
  minimum_cost_path_best: int64;
begin
  minimum_cost_path_rows := Length(minimum_cost_path_matrix);
  minimum_cost_path_cols := Length(minimum_cost_path_matrix[0]);
  minimum_cost_path_j := 1;
  while minimum_cost_path_j < minimum_cost_path_cols do begin
  minimum_cost_path_row0 := minimum_cost_path_matrix[0];
  minimum_cost_path_row0[minimum_cost_path_j] := minimum_cost_path_row0[minimum_cost_path_j] + minimum_cost_path_row0[minimum_cost_path_j - 1];
  minimum_cost_path_matrix[0] := minimum_cost_path_row0;
  minimum_cost_path_j := minimum_cost_path_j + 1;
end;
  minimum_cost_path_i := 1;
  while minimum_cost_path_i < minimum_cost_path_rows do begin
  minimum_cost_path_row := minimum_cost_path_matrix[minimum_cost_path_i];
  minimum_cost_path_row[0] := minimum_cost_path_row[0] + minimum_cost_path_matrix[minimum_cost_path_i - 1][0];
  minimum_cost_path_matrix[minimum_cost_path_i] := minimum_cost_path_row;
  minimum_cost_path_i := minimum_cost_path_i + 1;
end;
  minimum_cost_path_i := 1;
  while minimum_cost_path_i < minimum_cost_path_rows do begin
  minimum_cost_path_row := minimum_cost_path_matrix[minimum_cost_path_i];
  minimum_cost_path_j := 1;
  while minimum_cost_path_j < minimum_cost_path_cols do begin
  minimum_cost_path_up := minimum_cost_path_matrix[minimum_cost_path_i - 1][minimum_cost_path_j];
  minimum_cost_path_left := minimum_cost_path_row[minimum_cost_path_j - 1];
  minimum_cost_path_best := min_int(minimum_cost_path_up, minimum_cost_path_left);
  minimum_cost_path_row[minimum_cost_path_j] := minimum_cost_path_row[minimum_cost_path_j] + minimum_cost_path_best;
  minimum_cost_path_j := minimum_cost_path_j + 1;
end;
  minimum_cost_path_matrix[minimum_cost_path_i] := minimum_cost_path_row;
  minimum_cost_path_i := minimum_cost_path_i + 1;
end;
  exit(minimum_cost_path_matrix[minimum_cost_path_rows - 1][minimum_cost_path_cols - 1]);
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
  writeln('');
end.
