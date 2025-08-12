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
  r1: real;
  r2: real;
  r3: real;
  duration: real;
  coefficient_c: real;
  x: real;
  coefficient_k: real;
  exponent: real;
  base: real;
  coefficient_b: real;
  coefficient_a: real;
  return_period: real;
function exp_approx(x: real): real; forward;
function ln_series(x: real): real; forward;
function ln(x: real): real; forward;
function powf(base: real; exponent: real): real; forward;
function rainfall_intensity(coefficient_k: real; coefficient_a: real; coefficient_b: real; coefficient_c: real; return_period: real; duration: real): real; forward;
function exp_approx(x: real): real;
var
  exp_approx_y: real;
  exp_approx_is_neg: boolean;
  exp_approx_term: real;
  exp_approx_sum: real;
  exp_approx_n: integer;
begin
  exp_approx_y := x;
  exp_approx_is_neg := false;
  if x < 0 then begin
  exp_approx_is_neg := true;
  exp_approx_y := -x;
end;
  exp_approx_term := 1;
  exp_approx_sum := 1;
  exp_approx_n := 1;
  while exp_approx_n < 30 do begin
  exp_approx_term := (exp_approx_term * exp_approx_y) / Double(exp_approx_n);
  exp_approx_sum := exp_approx_sum + exp_approx_term;
  exp_approx_n := exp_approx_n + 1;
end;
  if exp_approx_is_neg then begin
  exit(1 / exp_approx_sum);
end;
  exit(exp_approx_sum);
end;
function ln_series(x: real): real;
var
  ln_series_t: real;
  ln_series_term: real;
  ln_series_sum: real;
  ln_series_n: integer;
begin
  ln_series_t := (x - 1) / (x + 1);
  ln_series_term := ln_series_t;
  ln_series_sum := 0;
  ln_series_n := 1;
  while ln_series_n <= 19 do begin
  ln_series_sum := ln_series_sum + (ln_series_term / Double(ln_series_n));
  ln_series_term := (ln_series_term * ln_series_t) * ln_series_t;
  ln_series_n := ln_series_n + 2;
end;
  exit(2 * ln_series_sum);
end;
function ln(x: real): real;
var
  ln_y: real;
  ln_k: integer;
begin
  ln_y := x;
  ln_k := 0;
  while ln_y >= 10 do begin
  ln_y := ln_y / 10;
  ln_k := ln_k + 1;
end;
  while ln_y < 1 do begin
  ln_y := ln_y * 10;
  ln_k := ln_k - 1;
end;
  exit(ln_series(ln_y) + (Double(ln_k) * ln_series(10)));
end;
function powf(base: real; exponent: real): real;
begin
  exit(exp_approx(exponent * ln(base)));
end;
function rainfall_intensity(coefficient_k: real; coefficient_a: real; coefficient_b: real; coefficient_c: real; return_period: real; duration: real): real;
var
  rainfall_intensity_numerator: real;
  rainfall_intensity_denominator: real;
begin
  if coefficient_k <= 0 then begin
  panic('All parameters must be positive.');
end;
  if coefficient_a <= 0 then begin
  panic('All parameters must be positive.');
end;
  if coefficient_b <= 0 then begin
  panic('All parameters must be positive.');
end;
  if coefficient_c <= 0 then begin
  panic('All parameters must be positive.');
end;
  if return_period <= 0 then begin
  panic('All parameters must be positive.');
end;
  if duration <= 0 then begin
  panic('All parameters must be positive.');
end;
  rainfall_intensity_numerator := coefficient_k * powf(return_period, coefficient_a);
  rainfall_intensity_denominator := powf(duration + coefficient_b, coefficient_c);
  exit(rainfall_intensity_numerator / rainfall_intensity_denominator);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  r1 := rainfall_intensity(1000, 0.2, 11.6, 0.81, 10, 60);
  writeln(FloatToStr(r1));
  r2 := rainfall_intensity(1000, 0.2, 11.6, 0.81, 10, 30);
  writeln(FloatToStr(r2));
  r3 := rainfall_intensity(1000, 0.2, 11.6, 0.81, 5, 60);
  writeln(FloatToStr(r3));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
