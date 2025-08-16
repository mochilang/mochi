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
  b: IntArrayArray;
  k: integer;
  matrix: IntArrayArray;
  n: integer;
  a: IntArrayArray;
  i: integer;
  j: integer;
  m: integer;
  result_: IntArrayArray;
function is_square(matrix: IntArrayArray): boolean; forward;
function matrix_multiply(a: IntArrayArray; b: IntArrayArray): IntArrayArray; forward;
procedure multiply(i: integer; j: integer; k: integer; a: IntArrayArray; b: IntArrayArray; multiply_result_: IntArrayArray; n: integer; m: integer); forward;
function matrix_multiply_recursive(a: IntArrayArray; b: IntArrayArray): IntArrayArray; forward;
function is_square(matrix: IntArrayArray): boolean;
var
  is_square_n: integer;
  is_square_i: integer;
begin
  is_square_n := Length(matrix);
  is_square_i := 0;
  while is_square_i < is_square_n do begin
  if Length(matrix[is_square_i]) <> is_square_n then begin
  exit(false);
end;
  is_square_i := is_square_i + 1;
end;
  exit(true);
end;
function matrix_multiply(a: IntArrayArray; b: IntArrayArray): IntArrayArray;
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
  matrix_multiply_rows := Length(a);
  matrix_multiply_cols := Length(b[0]);
  matrix_multiply_inner := Length(b);
  matrix_multiply_result_ := [];
  matrix_multiply_i := 0;
  while matrix_multiply_i < matrix_multiply_rows do begin
  matrix_multiply_row := [];
  matrix_multiply_j := 0;
  while matrix_multiply_j < matrix_multiply_cols do begin
  matrix_multiply_sum := 0;
  matrix_multiply_k := 0;
  while matrix_multiply_k < matrix_multiply_inner do begin
  matrix_multiply_sum := matrix_multiply_sum + (a[matrix_multiply_i][matrix_multiply_k] * b[matrix_multiply_k][matrix_multiply_j]);
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
procedure multiply(i: integer; j: integer; k: integer; a: IntArrayArray; b: IntArrayArray; multiply_result_: IntArrayArray; n: integer; m: integer);
begin
  if i >= n then begin
  exit();
end;
  if j >= m then begin
  multiply(i + 1, 0, 0, a, b, result_, n, m);
  exit();
end;
  if k >= Length(b) then begin
  multiply(i, j + 1, 0, a, b, result_, n, m);
  exit();
end;
  result_[i][j] := result_[i][j] + (a[i][k] * b[k][j]);
  multiply(i, j, k + 1, a, b, result_, n, m);
end;
function matrix_multiply_recursive(a: IntArrayArray; b: IntArrayArray): IntArrayArray;
var
  matrix_multiply_recursive_n: integer;
  matrix_multiply_recursive_m: integer;
  matrix_multiply_recursive_result_: array of IntArray;
  matrix_multiply_recursive_i: integer;
  matrix_multiply_recursive_row: array of integer;
  matrix_multiply_recursive_j: integer;
begin
  if (Length(a) = 0) or (Length(b) = 0) then begin
  exit([]);
end;
  if ((Length(a) <> Length(b)) or not is_square(a)) or not is_square(b) then begin
  panic('Invalid matrix dimensions');
end;
  matrix_multiply_recursive_n := Length(a);
  matrix_multiply_recursive_m := Length(b[0]);
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
  multiply(0, 0, 0, a, b, matrix_multiply_recursive_result_, matrix_multiply_recursive_n, matrix_multiply_recursive_m);
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
