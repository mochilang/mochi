{$mode objfpc}{$modeswitch nestedprocvars}
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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
procedure show_list_list(xs: array of IntArray);
var i: integer;
begin
  for i := 0 to High(xs) do begin
    show_list(xs[i]);
    if i < High(xs) then write(' ');
  end;
  writeln('');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  matrix_1_to_4: array of IntArray;
  matrix_5_to_8: array of IntArray;
  matrix_count_up: array of IntArray;
  matrix_unordered: array of IntArray;
function is_square(is_square_matrix: IntArrayArray): boolean; forward;
function matrix_multiply(matrix_multiply_a: IntArrayArray; matrix_multiply_b: IntArrayArray): IntArrayArray; forward;
procedure multiply(multiply_i: integer; multiply_j: integer; multiply_k: integer; multiply_a: IntArrayArray; multiply_b: IntArrayArray; multiply_result_: IntArrayArray; multiply_n: integer; multiply_m: integer); forward;
function matrix_multiply_recursive(matrix_multiply_recursive_a: IntArrayArray; matrix_multiply_recursive_b: IntArrayArray): IntArrayArray; forward;
function is_square(is_square_matrix: IntArrayArray): boolean;
var
  is_square_n: integer;
  is_square_i: integer;
begin
  is_square_n := Length(is_square_matrix);
  is_square_i := 0;
  while is_square_i < is_square_n do begin
  if Length(is_square_matrix[is_square_i]) <> is_square_n then begin
  exit(false);
end;
  is_square_i := is_square_i + 1;
end;
  exit(true);
end;
function matrix_multiply(matrix_multiply_a: IntArrayArray; matrix_multiply_b: IntArrayArray): IntArrayArray;
var
  matrix_multiply_rows: integer;
  matrix_multiply_cols: integer;
  matrix_multiply_inner: integer;
  matrix_multiply_result_: array of IntArray;
  matrix_multiply_i: integer;
  matrix_multiply_row: array of integer;
  matrix_multiply_j: integer;
  matrix_multiply_sum: integer;
  matrix_multiply_k: integer;
begin
  matrix_multiply_rows := Length(matrix_multiply_a);
  matrix_multiply_cols := Length(matrix_multiply_b[0]);
  matrix_multiply_inner := Length(matrix_multiply_b);
  matrix_multiply_result_ := [];
  matrix_multiply_i := 0;
  while matrix_multiply_i < matrix_multiply_rows do begin
  matrix_multiply_row := [];
  matrix_multiply_j := 0;
  while matrix_multiply_j < matrix_multiply_cols do begin
  matrix_multiply_sum := 0;
  matrix_multiply_k := 0;
  while matrix_multiply_k < matrix_multiply_inner do begin
  matrix_multiply_sum := matrix_multiply_sum + (matrix_multiply_a[matrix_multiply_i][matrix_multiply_k] * matrix_multiply_b[matrix_multiply_k][matrix_multiply_j]);
  matrix_multiply_k := matrix_multiply_k + 1;
end;
  matrix_multiply_row := concat(matrix_multiply_row, IntArray([matrix_multiply_sum]));
  matrix_multiply_j := matrix_multiply_j + 1;
end;
  matrix_multiply_result_ := concat(matrix_multiply_result_, [matrix_multiply_row]);
  matrix_multiply_i := matrix_multiply_i + 1;
end;
  exit(matrix_multiply_result_);
end;
procedure multiply(multiply_i: integer; multiply_j: integer; multiply_k: integer; multiply_a: IntArrayArray; multiply_b: IntArrayArray; multiply_result_: IntArrayArray; multiply_n: integer; multiply_m: integer);
begin
  if multiply_i >= multiply_n then begin
  exit();
end;
  if multiply_j >= multiply_m then begin
  multiply(multiply_i + 1, 0, 0, multiply_a, multiply_b, multiply_result_, multiply_n, multiply_m);
  exit();
end;
  if multiply_k >= Length(multiply_b) then begin
  multiply(multiply_i, multiply_j + 1, 0, multiply_a, multiply_b, multiply_result_, multiply_n, multiply_m);
  exit();
end;
  multiply_result_[multiply_i][multiply_j] := multiply_result_[multiply_i][multiply_j] + (multiply_a[multiply_i][multiply_k] * multiply_b[multiply_k][multiply_j]);
  multiply(multiply_i, multiply_j, multiply_k + 1, multiply_a, multiply_b, multiply_result_, multiply_n, multiply_m);
end;
function matrix_multiply_recursive(matrix_multiply_recursive_a: IntArrayArray; matrix_multiply_recursive_b: IntArrayArray): IntArrayArray;
var
  matrix_multiply_recursive_n: integer;
  matrix_multiply_recursive_m: integer;
  matrix_multiply_recursive_result_: array of IntArray;
  matrix_multiply_recursive_i: integer;
  matrix_multiply_recursive_row: array of integer;
  matrix_multiply_recursive_j: integer;
begin
  if (Length(matrix_multiply_recursive_a) = 0) or (Length(matrix_multiply_recursive_b) = 0) then begin
  exit([]);
end;
  if ((Length(matrix_multiply_recursive_a) <> Length(matrix_multiply_recursive_b)) or not is_square(matrix_multiply_recursive_a)) or not is_square(matrix_multiply_recursive_b) then begin
  panic('Invalid matrix dimensions');
end;
  matrix_multiply_recursive_n := Length(matrix_multiply_recursive_a);
  matrix_multiply_recursive_m := Length(matrix_multiply_recursive_b[0]);
  matrix_multiply_recursive_result_ := [];
  matrix_multiply_recursive_i := 0;
  while matrix_multiply_recursive_i < matrix_multiply_recursive_n do begin
  matrix_multiply_recursive_row := [];
  matrix_multiply_recursive_j := 0;
  while matrix_multiply_recursive_j < matrix_multiply_recursive_m do begin
  matrix_multiply_recursive_row := concat(matrix_multiply_recursive_row, IntArray([0]));
  matrix_multiply_recursive_j := matrix_multiply_recursive_j + 1;
end;
  matrix_multiply_recursive_result_ := concat(matrix_multiply_recursive_result_, [matrix_multiply_recursive_row]);
  matrix_multiply_recursive_i := matrix_multiply_recursive_i + 1;
end;
  multiply(0, 0, 0, matrix_multiply_recursive_a, matrix_multiply_recursive_b, matrix_multiply_recursive_result_, matrix_multiply_recursive_n, matrix_multiply_recursive_m);
  exit(matrix_multiply_recursive_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  matrix_1_to_4 := [[1, 2], [3, 4]];
  matrix_5_to_8 := [[5, 6], [7, 8]];
  matrix_count_up := [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]];
  matrix_unordered := [[5, 8, 1, 2], [6, 7, 3, 0], [4, 5, 9, 1], [2, 6, 10, 14]];
  show_list_list(matrix_multiply_recursive(matrix_1_to_4, matrix_5_to_8));
  show_list_list(matrix_multiply_recursive(matrix_count_up, matrix_unordered));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
