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
  cols: integer;
  arr: IntArray;
  rows: integer;
  required_sum: integer;
function create_bool_matrix(rows: integer; cols: integer): BoolArrayArray; forward;
function is_sum_subset(arr: IntArray; required_sum: integer): boolean; forward;
function create_bool_matrix(rows: integer; cols: integer): BoolArrayArray;
var
  create_bool_matrix_matrix: array of BoolArray;
  create_bool_matrix_i: integer;
  create_bool_matrix_row: array of boolean;
  create_bool_matrix_j: integer;
begin
  create_bool_matrix_matrix := [];
  create_bool_matrix_i := 0;
  while create_bool_matrix_i <= rows do begin
  create_bool_matrix_row := [];
  create_bool_matrix_j := 0;
  while create_bool_matrix_j <= cols do begin
  create_bool_matrix_row := concat(create_bool_matrix_row, [false]);
  create_bool_matrix_j := create_bool_matrix_j + 1;
end;
  create_bool_matrix_matrix := concat(create_bool_matrix_matrix, [create_bool_matrix_row]);
  create_bool_matrix_i := create_bool_matrix_i + 1;
end;
  exit(create_bool_matrix_matrix);
end;
function is_sum_subset(arr: IntArray; required_sum: integer): boolean;
var
  is_sum_subset_arr_len: integer;
  is_sum_subset_subset: array of BoolArray;
  is_sum_subset_i: integer;
  is_sum_subset_j: integer;
begin
  is_sum_subset_arr_len := Length(arr);
  is_sum_subset_subset := create_bool_matrix(is_sum_subset_arr_len, required_sum);
  is_sum_subset_i := 0;
  while is_sum_subset_i <= is_sum_subset_arr_len do begin
  is_sum_subset_subset[is_sum_subset_i][0] := true;
  is_sum_subset_i := is_sum_subset_i + 1;
end;
  is_sum_subset_j := 1;
  while is_sum_subset_j <= required_sum do begin
  is_sum_subset_subset[0][is_sum_subset_j] := false;
  is_sum_subset_j := is_sum_subset_j + 1;
end;
  is_sum_subset_i := 1;
  while is_sum_subset_i <= is_sum_subset_arr_len do begin
  is_sum_subset_j := 1;
  while is_sum_subset_j <= required_sum do begin
  if arr[is_sum_subset_i - 1] > is_sum_subset_j then begin
  is_sum_subset_subset[is_sum_subset_i][is_sum_subset_j] := is_sum_subset_subset[is_sum_subset_i - 1][is_sum_subset_j];
end;
  if arr[is_sum_subset_i - 1] <= is_sum_subset_j then begin
  is_sum_subset_subset[is_sum_subset_i][is_sum_subset_j] := is_sum_subset_subset[is_sum_subset_i - 1][is_sum_subset_j] or is_sum_subset_subset[is_sum_subset_i - 1][is_sum_subset_j - arr[is_sum_subset_i - 1]];
end;
  is_sum_subset_j := is_sum_subset_j + 1;
end;
  is_sum_subset_i := is_sum_subset_i + 1;
end;
  exit(is_sum_subset_subset[is_sum_subset_arr_len][required_sum]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(Ord(is_sum_subset([2, 4, 6, 8], 5)));
  writeln(Ord(is_sum_subset([2, 4, 6, 8], 14)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
