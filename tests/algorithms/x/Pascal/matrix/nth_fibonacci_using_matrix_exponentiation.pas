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
function multiply(multiply_matrix_a: IntArrayArray; multiply_matrix_b: IntArrayArray): IntArrayArray; forward;
function identity(identity_n: int64): IntArrayArray; forward;
function nth_fibonacci_matrix(nth_fibonacci_matrix_n: int64): int64; forward;
function nth_fibonacci_bruteforce(nth_fibonacci_bruteforce_n: int64): int64; forward;
function parse_number(parse_number_s: string): int64; forward;
procedure main(); forward;
function multiply(multiply_matrix_a: IntArrayArray; multiply_matrix_b: IntArrayArray): IntArrayArray;
var
  multiply_n: integer;
  multiply_matrix_c: array of IntArray;
  multiply_i: int64;
  multiply_row: array of int64;
  multiply_j: int64;
  multiply_val: int64;
  multiply_k: int64;
begin
  multiply_n := Length(multiply_matrix_a);
  multiply_matrix_c := [];
  multiply_i := 0;
  while multiply_i < multiply_n do begin
  multiply_row := [];
  multiply_j := 0;
  while multiply_j < multiply_n do begin
  multiply_val := 0;
  multiply_k := 0;
  while multiply_k < multiply_n do begin
  multiply_val := multiply_val + (multiply_matrix_a[multiply_i][multiply_k] * multiply_matrix_b[multiply_k][multiply_j]);
  multiply_k := multiply_k + 1;
end;
  multiply_row := concat(multiply_row, IntArray([multiply_val]));
  multiply_j := multiply_j + 1;
end;
  multiply_matrix_c := concat(multiply_matrix_c, [multiply_row]);
  multiply_i := multiply_i + 1;
end;
  exit(multiply_matrix_c);
end;
function identity(identity_n: int64): IntArrayArray;
var
  identity_res: array of IntArray;
  identity_i: int64;
  identity_row: array of int64;
  identity_j: int64;
begin
  identity_res := [];
  identity_i := 0;
  while identity_i < identity_n do begin
  identity_row := [];
  identity_j := 0;
  while identity_j < identity_n do begin
  if identity_i = identity_j then begin
  identity_row := concat(identity_row, IntArray([1]));
end else begin
  identity_row := concat(identity_row, IntArray([0]));
end;
  identity_j := identity_j + 1;
end;
  identity_res := concat(identity_res, [identity_row]);
  identity_i := identity_i + 1;
end;
  exit(identity_res);
end;
function nth_fibonacci_matrix(nth_fibonacci_matrix_n: int64): int64;
var
  nth_fibonacci_matrix_res_matrix: IntArrayArray;
  nth_fibonacci_matrix_fib_matrix: array of array of int64;
  nth_fibonacci_matrix_m: int64;
begin
  if nth_fibonacci_matrix_n <= 1 then begin
  exit(nth_fibonacci_matrix_n);
end;
  nth_fibonacci_matrix_res_matrix := identity(2);
  nth_fibonacci_matrix_fib_matrix := [[1, 1], [1, 0]];
  nth_fibonacci_matrix_m := nth_fibonacci_matrix_n - 1;
  while nth_fibonacci_matrix_m > 0 do begin
  if (nth_fibonacci_matrix_m mod 2) = 1 then begin
  nth_fibonacci_matrix_res_matrix := multiply(nth_fibonacci_matrix_res_matrix, nth_fibonacci_matrix_fib_matrix);
end;
  nth_fibonacci_matrix_fib_matrix := multiply(nth_fibonacci_matrix_fib_matrix, nth_fibonacci_matrix_fib_matrix);
  nth_fibonacci_matrix_m := _floordiv(nth_fibonacci_matrix_m, 2);
end;
  exit(nth_fibonacci_matrix_res_matrix[0][0]);
end;
function nth_fibonacci_bruteforce(nth_fibonacci_bruteforce_n: int64): int64;
var
  nth_fibonacci_bruteforce_fib0: int64;
  nth_fibonacci_bruteforce_fib1: int64;
  nth_fibonacci_bruteforce_i: int64;
  nth_fibonacci_bruteforce_next: int64;
begin
  if nth_fibonacci_bruteforce_n <= 1 then begin
  exit(nth_fibonacci_bruteforce_n);
end;
  nth_fibonacci_bruteforce_fib0 := 0;
  nth_fibonacci_bruteforce_fib1 := 1;
  nth_fibonacci_bruteforce_i := 2;
  while nth_fibonacci_bruteforce_i <= nth_fibonacci_bruteforce_n do begin
  nth_fibonacci_bruteforce_next := nth_fibonacci_bruteforce_fib0 + nth_fibonacci_bruteforce_fib1;
  nth_fibonacci_bruteforce_fib0 := nth_fibonacci_bruteforce_fib1;
  nth_fibonacci_bruteforce_fib1 := nth_fibonacci_bruteforce_next;
  nth_fibonacci_bruteforce_i := nth_fibonacci_bruteforce_i + 1;
end;
  exit(nth_fibonacci_bruteforce_fib1);
end;
function parse_number(parse_number_s: string): int64;
var
  parse_number_result_: int64;
  parse_number_i: int64;
  parse_number_ch: string;
begin
  parse_number_result_ := 0;
  parse_number_i := 0;
  while parse_number_i < Length(parse_number_s) do begin
  parse_number_ch := copy(parse_number_s, parse_number_i+1, (parse_number_i + 1 - (parse_number_i)));
  if (parse_number_ch >= '0') and (parse_number_ch <= '9') then begin
  parse_number_result_ := (parse_number_result_ * 10) + StrToInt(parse_number_ch);
end;
  parse_number_i := parse_number_i + 1;
end;
  exit(parse_number_result_);
end;
procedure main();
var
  main_ordinals: array of string;
  main_i: int64;
  main_ordinal: string;
  main_n: int64;
  main_msg: string;
begin
  main_ordinals := ['0th', '1st', '2nd', '3rd', '10th', '100th', '1000th'];
  main_i := 0;
  while main_i < Length(main_ordinals) do begin
  main_ordinal := main_ordinals[main_i];
  main_n := parse_number(main_ordinal);
  main_msg := (((main_ordinal + ' fibonacci number using matrix exponentiation is ') + IntToStr(nth_fibonacci_matrix(main_n))) + ' and using bruteforce is ') + IntToStr(nth_fibonacci_bruteforce(main_n));
  writeln(main_msg);
  main_i := main_i + 1;
end;
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
