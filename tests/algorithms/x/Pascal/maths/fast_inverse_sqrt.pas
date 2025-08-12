{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
  rel_tol: real;
  k: integer;
  a: real;
  num: integer;
  b: real;
  x: real;
  n: integer;
  bits: integer;
  number: real;
function pow2_int(n: integer): integer; forward;
function pow2_float(n: integer): real; forward;
function lshift(num: integer; k: integer): integer; forward;
function rshift(num: integer; k: integer): integer; forward;
function log2_floor(x: real): integer; forward;
function float_to_bits(x: real): integer; forward;
function bits_to_float(bits: integer): real; forward;
function absf(x: real): real; forward;
function sqrtApprox(x: real): real; forward;
function is_close(a: real; b: real; rel_tol: real): boolean; forward;
function fast_inverse_sqrt(number: real): real; forward;
procedure test_fast_inverse_sqrt(); forward;
procedure main(); forward;
function pow2_int(n: integer): integer;
var
  pow2_int_result_: integer;
  pow2_int_i: integer;
begin
  pow2_int_result_ := 1;
  pow2_int_i := 0;
  while pow2_int_i < n do begin
  pow2_int_result_ := pow2_int_result_ * 2;
  pow2_int_i := pow2_int_i + 1;
end;
  exit(pow2_int_result_);
end;
function pow2_float(n: integer): real;
var
  pow2_float_result_: real;
  pow2_float_i: integer;
  pow2_float_m: integer;
begin
  pow2_float_result_ := 1;
  if n >= 0 then begin
  pow2_float_i := 0;
  while pow2_float_i < n do begin
  pow2_float_result_ := pow2_float_result_ * 2;
  pow2_float_i := pow2_float_i + 1;
end;
end else begin
  pow2_float_i := 0;
  pow2_float_m := 0 - n;
  while pow2_float_i < pow2_float_m do begin
  pow2_float_result_ := pow2_float_result_ / 2;
  pow2_float_i := pow2_float_i + 1;
end;
end;
  exit(pow2_float_result_);
end;
function lshift(num: integer; k: integer): integer;
var
  lshift_result_: integer;
  lshift_i: integer;
begin
  lshift_result_ := num;
  lshift_i := 0;
  while lshift_i < k do begin
  lshift_result_ := lshift_result_ * 2;
  lshift_i := lshift_i + 1;
end;
  exit(lshift_result_);
end;
function rshift(num: integer; k: integer): integer;
var
  rshift_result_: integer;
  rshift_i: integer;
begin
  rshift_result_ := num;
  rshift_i := 0;
  while rshift_i < k do begin
  rshift_result_ := (rshift_result_ - (rshift_result_ mod 2)) div 2;
  rshift_i := rshift_i + 1;
end;
  exit(rshift_result_);
end;
function log2_floor(x: real): integer;
var
  log2_floor_n: real;
  log2_floor_e: integer;
begin
  log2_floor_n := x;
  log2_floor_e := 0;
  while log2_floor_n >= 2 do begin
  log2_floor_n := log2_floor_n / 2;
  log2_floor_e := log2_floor_e + 1;
end;
  while log2_floor_n < 1 do begin
  log2_floor_n := log2_floor_n * 2;
  log2_floor_e := log2_floor_e - 1;
end;
  exit(log2_floor_e);
end;
function float_to_bits(x: real): integer;
var
  float_to_bits_num: real;
  float_to_bits_sign: integer;
  float_to_bits_exp: integer;
  float_to_bits_pow: real;
  float_to_bits_normalized: real;
  float_to_bits_frac: real;
  float_to_bits_mantissa: integer;
  float_to_bits_exp_bits: integer;
begin
  float_to_bits_num := x;
  float_to_bits_sign := 0;
  if float_to_bits_num < 0 then begin
  float_to_bits_sign := 1;
  float_to_bits_num := -float_to_bits_num;
end;
  float_to_bits_exp := log2_floor(float_to_bits_num);
  float_to_bits_pow := pow2_float(float_to_bits_exp);
  float_to_bits_normalized := float_to_bits_num / float_to_bits_pow;
  float_to_bits_frac := float_to_bits_normalized - 1;
  float_to_bits_mantissa := Trunc(float_to_bits_frac * pow2_float(23));
  float_to_bits_exp_bits := float_to_bits_exp + 127;
  exit((lshift(float_to_bits_sign, 31) + lshift(float_to_bits_exp_bits, 23)) + float_to_bits_mantissa);
end;
function bits_to_float(bits: integer): real;
var
  bits_to_float_sign_bit: integer;
  bits_to_float_sign: real;
  bits_to_float_exp_bits: integer;
  bits_to_float_exp: integer;
  bits_to_float_mantissa_bits: integer;
  bits_to_float_mantissa: real;
begin
  bits_to_float_sign_bit := rshift(bits, 31) mod 2;
  bits_to_float_sign := 1;
  if bits_to_float_sign_bit = 1 then begin
  bits_to_float_sign := -1;
end;
  bits_to_float_exp_bits := rshift(bits, 23) mod 256;
  bits_to_float_exp := bits_to_float_exp_bits - 127;
  bits_to_float_mantissa_bits := bits mod pow2_int(23);
  bits_to_float_mantissa := 1 + (Double(bits_to_float_mantissa_bits) / pow2_float(23));
  exit((bits_to_float_sign * bits_to_float_mantissa) * pow2_float(bits_to_float_exp));
end;
function absf(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrtApprox_guess := x / 2;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function is_close(a: real; b: real; rel_tol: real): boolean;
begin
  exit(absf(a - b) <= (rel_tol * absf(b)));
end;
function fast_inverse_sqrt(number: real): real;
var
  fast_inverse_sqrt_i: integer;
  fast_inverse_sqrt_magic: integer;
  fast_inverse_sqrt_y_bits: integer;
  fast_inverse_sqrt_y: real;
begin
  if number <= 0 then begin
  panic('Input must be a positive number.');
end;
  fast_inverse_sqrt_i := float_to_bits(number);
  fast_inverse_sqrt_magic := 1597463007;
  fast_inverse_sqrt_y_bits := fast_inverse_sqrt_magic - rshift(fast_inverse_sqrt_i, 1);
  fast_inverse_sqrt_y := bits_to_float(fast_inverse_sqrt_y_bits);
  fast_inverse_sqrt_y := fast_inverse_sqrt_y * (1.5 - (((0.5 * number) * fast_inverse_sqrt_y) * fast_inverse_sqrt_y));
  exit(fast_inverse_sqrt_y);
end;
procedure test_fast_inverse_sqrt();
var
  test_fast_inverse_sqrt_i: integer;
  test_fast_inverse_sqrt_y: real;
  test_fast_inverse_sqrt_actual: real;
begin
  if absf(fast_inverse_sqrt(10) - 0.3156857923527257) > 0.0001 then begin
  panic('fast_inverse_sqrt(10) failed');
end;
  if absf(fast_inverse_sqrt(4) - 0.49915357479239103) > 0.0001 then begin
  panic('fast_inverse_sqrt(4) failed');
end;
  if absf(fast_inverse_sqrt(4.1) - 0.4932849504615651) > 0.0001 then begin
  panic('fast_inverse_sqrt(4.1) failed');
end;
  test_fast_inverse_sqrt_i := 50;
  while test_fast_inverse_sqrt_i < 60 do begin
  test_fast_inverse_sqrt_y := fast_inverse_sqrt(Double(test_fast_inverse_sqrt_i));
  test_fast_inverse_sqrt_actual := 1 / sqrtApprox(Double(test_fast_inverse_sqrt_i));
  if not is_close(test_fast_inverse_sqrt_y, test_fast_inverse_sqrt_actual, 0.00132) then begin
  panic('relative error too high');
end;
  test_fast_inverse_sqrt_i := test_fast_inverse_sqrt_i + 1;
end;
end;
procedure main();
var
  main_i: integer;
  main_diff: real;
begin
  test_fast_inverse_sqrt();
  main_i := 5;
  while main_i <= 100 do begin
  main_diff := (1 / sqrtApprox(Double(main_i))) - fast_inverse_sqrt(Double(main_i));
  writeln((IntToStr(main_i) + ': ') + FloatToStr(main_diff));
  main_i := main_i + 5;
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
end.
