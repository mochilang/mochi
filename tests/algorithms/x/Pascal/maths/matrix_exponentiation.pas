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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  f1: integer;
  n: integer;
  exp: integer;
  b: IntArrayArray;
  base: IntArrayArray;
  f2: integer;
  a: IntArrayArray;
function identity(n: integer): IntArrayArray; forward;
function matrix_mul(a: IntArrayArray; b: IntArrayArray): IntArrayArray; forward;
function matrix_pow(base: IntArrayArray; exp: integer): IntArrayArray; forward;
function fibonacci_with_matrix_exponentiation(n: integer; f1: integer; f2: integer): integer; forward;
function simple_fibonacci(n: integer; f1: integer; f2: integer): integer; forward;
function identity(n: integer): IntArrayArray;
var
  identity_i: integer;
  identity_mat: array of IntArray;
  identity_row: array of integer;
  identity_j: integer;
begin
  identity_i := 0;
  identity_mat := [];
  while identity_i < n do begin
  identity_row := [];
  identity_j := 0;
  while identity_j < n do begin
  if identity_i = identity_j then begin
  identity_row := concat(identity_row, IntArray([1]));
end else begin
  identity_row := concat(identity_row, IntArray([0]));
end;
  identity_j := identity_j + 1;
end;
  identity_mat := concat(identity_mat, [identity_row]);
  identity_i := identity_i + 1;
end;
  exit(identity_mat);
end;
function matrix_mul(a: IntArrayArray; b: IntArrayArray): IntArrayArray;
var
  matrix_mul_n: integer;
  matrix_mul_result_: array of IntArray;
  matrix_mul_i: integer;
  matrix_mul_row: array of integer;
  matrix_mul_j: integer;
  matrix_mul_cell: integer;
  matrix_mul_k: integer;
begin
  matrix_mul_n := Length(a);
  matrix_mul_result_ := [];
  matrix_mul_i := 0;
  while matrix_mul_i < matrix_mul_n do begin
  matrix_mul_row := [];
  matrix_mul_j := 0;
  while matrix_mul_j < matrix_mul_n do begin
  matrix_mul_cell := 0;
  matrix_mul_k := 0;
  while matrix_mul_k < matrix_mul_n do begin
  matrix_mul_cell := matrix_mul_cell + (a[matrix_mul_i][matrix_mul_k] * b[matrix_mul_k][matrix_mul_j]);
  matrix_mul_k := matrix_mul_k + 1;
end;
  matrix_mul_row := concat(matrix_mul_row, IntArray([matrix_mul_cell]));
  matrix_mul_j := matrix_mul_j + 1;
end;
  matrix_mul_result_ := concat(matrix_mul_result_, [matrix_mul_row]);
  matrix_mul_i := matrix_mul_i + 1;
end;
  exit(matrix_mul_result_);
end;
function matrix_pow(base: IntArrayArray; exp: integer): IntArrayArray;
var
  matrix_pow_result_: IntArrayArray;
  matrix_pow_b: array of IntArray;
  matrix_pow_e: integer;
begin
  matrix_pow_result_ := identity(Length(base));
  matrix_pow_b := base;
  matrix_pow_e := exp;
  while matrix_pow_e > 0 do begin
  if (matrix_pow_e mod 2) = 1 then begin
  matrix_pow_result_ := matrix_mul(matrix_pow_result_, matrix_pow_b);
end;
  matrix_pow_b := matrix_mul(matrix_pow_b, matrix_pow_b);
  matrix_pow_e := matrix_pow_e div 2;
end;
  exit(matrix_pow_result_);
end;
function fibonacci_with_matrix_exponentiation(n: integer; f1: integer; f2: integer): integer;
var
  fibonacci_with_matrix_exponentiation_base: array of IntArray;
  fibonacci_with_matrix_exponentiation_m: IntArrayArray;
begin
  if n = 1 then begin
  exit(f1);
end;
  if n = 2 then begin
  exit(f2);
end;
  fibonacci_with_matrix_exponentiation_base := [[1, 1], [1, 0]];
  fibonacci_with_matrix_exponentiation_m := matrix_pow(fibonacci_with_matrix_exponentiation_base, n - 2);
  exit((f2 * fibonacci_with_matrix_exponentiation_m[0][0]) + (f1 * fibonacci_with_matrix_exponentiation_m[0][1]));
end;
function simple_fibonacci(n: integer; f1: integer; f2: integer): integer;
var
  simple_fibonacci_a: integer;
  simple_fibonacci_b: integer;
  simple_fibonacci_count: integer;
  simple_fibonacci_tmp: integer;
begin
  if n = 1 then begin
  exit(f1);
end;
  if n = 2 then begin
  exit(f2);
end;
  simple_fibonacci_a := f1;
  simple_fibonacci_b := f2;
  simple_fibonacci_count := n - 2;
  while simple_fibonacci_count > 0 do begin
  simple_fibonacci_tmp := simple_fibonacci_a + simple_fibonacci_b;
  simple_fibonacci_a := simple_fibonacci_b;
  simple_fibonacci_b := simple_fibonacci_tmp;
  simple_fibonacci_count := simple_fibonacci_count - 1;
end;
  exit(simple_fibonacci_b);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(fibonacci_with_matrix_exponentiation(1, 5, 6)));
  writeln(IntToStr(fibonacci_with_matrix_exponentiation(2, 10, 11)));
  writeln(IntToStr(fibonacci_with_matrix_exponentiation(13, 0, 1)));
  writeln(IntToStr(fibonacci_with_matrix_exponentiation(10, 5, 9)));
  writeln(IntToStr(fibonacci_with_matrix_exponentiation(9, 2, 3)));
  writeln(IntToStr(simple_fibonacci(1, 5, 6)));
  writeln(IntToStr(simple_fibonacci(2, 10, 11)));
  writeln(IntToStr(simple_fibonacci(13, 0, 1)));
  writeln(IntToStr(simple_fibonacci(10, 5, 9)));
  writeln(IntToStr(simple_fibonacci(9, 2, 3)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
