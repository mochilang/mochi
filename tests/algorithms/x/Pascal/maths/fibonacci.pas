{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
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
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  fib_cache_global: specialize TFPGMap<integer, integer>;
  fib_memo_cache: specialize TFPGMap<integer, integer>;
  m: IntArrayArray;
  a: IntArrayArray;
  num: integer;
  n: integer;
  x: real;
  i: integer;
  b: IntArrayArray;
  power: integer;
function Map1(): specialize TFPGMap<integer, integer>; forward;
function sqrt(x: real): real; forward;
function powf(x: real; n: integer): real; forward;
function roundf(x: real): integer; forward;
function fib_iterative(n: integer): IntArray; forward;
function fib_recursive_term(i: integer): integer; forward;
function fib_recursive(n: integer): IntArray; forward;
function fib_recursive_cached_term(i: integer): integer; forward;
function fib_recursive_cached(n: integer): IntArray; forward;
function fib_memoization_term(num: integer): integer; forward;
function fib_memoization(n: integer): IntArray; forward;
function fib_binet(n: integer): IntArray; forward;
function matrix_mul(a: IntArrayArray; b: IntArrayArray): IntArrayArray; forward;
function matrix_pow(m: IntArrayArray; power: integer): IntArrayArray; forward;
function fib_matrix(n: integer): integer; forward;
function run_tests(): integer; forward;
function Map1(): specialize TFPGMap<integer, integer>;
begin
  Result := specialize TFPGMap<integer, integer>.Create();
  Result.AddOrSetData(0, Variant(0));
  Result.AddOrSetData(1, Variant(1));
  Result.AddOrSetData(2, Variant(1));
end;
function sqrt(x: real): real;
var
  sqrt_guess: real;
  sqrt_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrt_guess := x;
  sqrt_i := 0;
  while sqrt_i < 10 do begin
  sqrt_guess := (sqrt_guess + (x / sqrt_guess)) / 2;
  sqrt_i := sqrt_i + 1;
end;
  exit(sqrt_guess);
end;
function powf(x: real; n: integer): real;
var
  powf_res: real;
  powf_i: integer;
begin
  powf_res := 1;
  powf_i := 0;
  while powf_i < n do begin
  powf_res := powf_res * x;
  powf_i := powf_i + 1;
end;
  exit(powf_res);
end;
function roundf(x: real): integer;
begin
  if x >= 0 then begin
  exit(Trunc(x + 0.5));
end;
  exit(Trunc(x - 0.5));
end;
function fib_iterative(n: integer): IntArray;
var
  fib_iterative_fib: array of integer;
  fib_iterative_i: integer;
begin
  if n < 0 then begin
  panic('n is negative');
end;
  if n = 0 then begin
  exit([0]);
end;
  fib_iterative_fib := [0, 1];
  fib_iterative_i := 2;
  while fib_iterative_i <= n do begin
  fib_iterative_fib := concat(fib_iterative_fib, IntArray([fib_iterative_fib[fib_iterative_i - 1] + fib_iterative_fib[fib_iterative_i - 2]]));
  fib_iterative_i := fib_iterative_i + 1;
end;
  exit(fib_iterative_fib);
end;
function fib_recursive_term(i: integer): integer;
begin
  if i < 0 then begin
  panic('n is negative');
end;
  if i < 2 then begin
  exit(i);
end;
  exit(fib_recursive_term(i - 1) + fib_recursive_term(i - 2));
end;
function fib_recursive(n: integer): IntArray;
var
  fib_recursive_res: array of integer;
  fib_recursive_i: integer;
begin
  if n < 0 then begin
  panic('n is negative');
end;
  fib_recursive_res := [];
  fib_recursive_i := 0;
  while fib_recursive_i <= n do begin
  fib_recursive_res := concat(fib_recursive_res, IntArray([fib_recursive_term(fib_recursive_i)]));
  fib_recursive_i := fib_recursive_i + 1;
end;
  exit(fib_recursive_res);
end;
function fib_recursive_cached_term(i: integer): integer;
var
  fib_recursive_cached_term_val: integer;
begin
  if i < 0 then begin
  panic('n is negative');
end;
  if i < 2 then begin
  exit(i);
end;
  if fib_cache_global.IndexOf(i) <> -1 then begin
  exit(fib_cache_global[i]);
end;
  fib_recursive_cached_term_val := fib_recursive_cached_term(i - 1) + fib_recursive_cached_term(i - 2);
  fib_cache_global[i] := fib_recursive_cached_term_val;
  exit(fib_recursive_cached_term_val);
end;
function fib_recursive_cached(n: integer): IntArray;
var
  fib_recursive_cached_res: array of integer;
  fib_recursive_cached_j: integer;
begin
  if n < 0 then begin
  panic('n is negative');
end;
  fib_recursive_cached_res := [];
  fib_recursive_cached_j := 0;
  while fib_recursive_cached_j <= n do begin
  fib_recursive_cached_res := concat(fib_recursive_cached_res, IntArray([fib_recursive_cached_term(fib_recursive_cached_j)]));
  fib_recursive_cached_j := fib_recursive_cached_j + 1;
end;
  exit(fib_recursive_cached_res);
end;
function fib_memoization_term(num: integer): integer;
var
  fib_memoization_term_value: integer;
begin
  if fib_memo_cache.IndexOf(num) <> -1 then begin
  exit(fib_memo_cache[num]);
end;
  fib_memoization_term_value := fib_memoization_term(num - 1) + fib_memoization_term(num - 2);
  fib_memo_cache[num] := fib_memoization_term_value;
  exit(fib_memoization_term_value);
end;
function fib_memoization(n: integer): IntArray;
var
  fib_memoization_out: array of integer;
  fib_memoization_i: integer;
begin
  if n < 0 then begin
  panic('n is negative');
end;
  fib_memoization_out := [];
  fib_memoization_i := 0;
  while fib_memoization_i <= n do begin
  fib_memoization_out := concat(fib_memoization_out, IntArray([fib_memoization_term(fib_memoization_i)]));
  fib_memoization_i := fib_memoization_i + 1;
end;
  exit(fib_memoization_out);
end;
function fib_binet(n: integer): IntArray;
var
  fib_binet_sqrt5: real;
  fib_binet_phi: real;
  fib_binet_res: array of integer;
  fib_binet_i: integer;
  fib_binet_val: integer;
begin
  if n < 0 then begin
  panic('n is negative');
end;
  if n >= 1475 then begin
  panic('n is too large');
end;
  fib_binet_sqrt5 := sqrt(5);
  fib_binet_phi := (1 + fib_binet_sqrt5) / 2;
  fib_binet_res := [];
  fib_binet_i := 0;
  while fib_binet_i <= n do begin
  fib_binet_val := roundf(powf(fib_binet_phi, fib_binet_i) / fib_binet_sqrt5);
  fib_binet_res := concat(fib_binet_res, IntArray([fib_binet_val]));
  fib_binet_i := fib_binet_i + 1;
end;
  exit(fib_binet_res);
end;
function matrix_mul(a: IntArrayArray; b: IntArrayArray): IntArrayArray;
var
  matrix_mul_a00: integer;
  matrix_mul_a01: integer;
  matrix_mul_a10: integer;
  matrix_mul_a11: integer;
begin
  matrix_mul_a00 := (a[0][0] * b[0][0]) + (a[0][1] * b[1][0]);
  matrix_mul_a01 := (a[0][0] * b[0][1]) + (a[0][1] * b[1][1]);
  matrix_mul_a10 := (a[1][0] * b[0][0]) + (a[1][1] * b[1][0]);
  matrix_mul_a11 := (a[1][0] * b[0][1]) + (a[1][1] * b[1][1]);
  exit([[matrix_mul_a00, matrix_mul_a01], [matrix_mul_a10, matrix_mul_a11]]);
end;
function matrix_pow(m: IntArrayArray; power: integer): IntArrayArray;
var
  matrix_pow_result_: array of IntArray;
  matrix_pow_base: array of IntArray;
  matrix_pow_p: integer;
begin
  if power < 0 then begin
  panic('power is negative');
end;
  matrix_pow_result_ := [[1, 0], [0, 1]];
  matrix_pow_base := m;
  matrix_pow_p := power;
  while matrix_pow_p > 0 do begin
  if (matrix_pow_p mod 2) = 1 then begin
  matrix_pow_result_ := matrix_mul(matrix_pow_result_, matrix_pow_base);
end;
  matrix_pow_base := matrix_mul(matrix_pow_base, matrix_pow_base);
  matrix_pow_p := Trunc(matrix_pow_p div 2);
end;
  exit(matrix_pow_result_);
end;
function fib_matrix(n: integer): integer;
var
  fib_matrix_m: array of array of integer;
  fib_matrix_res: IntArrayArray;
begin
  if n < 0 then begin
  panic('n is negative');
end;
  if n = 0 then begin
  exit(0);
end;
  fib_matrix_m := [[1, 1], [1, 0]];
  fib_matrix_res := matrix_pow(fib_matrix_m, n - 1);
  exit(fib_matrix_res[0][0]);
end;
function run_tests(): integer;
var
  run_tests_expected: array of integer;
  run_tests_it: IntArray;
  run_tests_rec: IntArray;
  run_tests_cache: IntArray;
  run_tests_memo: IntArray;
  run_tests_bin: IntArray;
  run_tests_m: integer;
begin
  run_tests_expected := [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55];
  run_tests_it := fib_iterative(10);
  run_tests_rec := fib_recursive(10);
  run_tests_cache := fib_recursive_cached(10);
  run_tests_memo := fib_memoization(10);
  run_tests_bin := fib_binet(10);
  run_tests_m := fib_matrix(10);
  if list_int_to_str(run_tests_it) <> list_int_to_str(run_tests_expected) then begin
  panic('iterative failed');
end;
  if list_int_to_str(run_tests_rec) <> list_int_to_str(run_tests_expected) then begin
  panic('recursive failed');
end;
  if list_int_to_str(run_tests_cache) <> list_int_to_str(run_tests_expected) then begin
  panic('cached failed');
end;
  if list_int_to_str(run_tests_memo) <> list_int_to_str(run_tests_expected) then begin
  panic('memoization failed');
end;
  if list_int_to_str(run_tests_bin) <> list_int_to_str(run_tests_expected) then begin
  panic('binet failed');
end;
  if run_tests_m <> 55 then begin
  panic('matrix failed');
end;
  exit(run_tests_m);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  fib_cache_global := specialize TFPGMap<integer, integer>.Create();
  fib_memo_cache := Map1();
  writeln(IntToStr(run_tests()));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
