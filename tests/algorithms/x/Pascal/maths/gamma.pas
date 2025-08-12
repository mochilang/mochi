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
  PI: real;
  exponent: real;
  num: real;
  x: real;
  base: real;
  z: real;
function absf(x: real): real; forward;
function sqrt(x: real): real; forward;
function ln(x: real): real; forward;
function exp_series(x: real): real; forward;
function powf(base: real; exponent: real): real; forward;
function integrand(x: real; z: real): real; forward;
function gamma_iterative(num: real): real; forward;
function gamma_recursive(num: real): real; forward;
procedure main(); forward;
function absf(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function sqrt(x: real): real;
var
  sqrt_guess: real;
  sqrt_i: integer;
begin
  if x < 0 then begin
  panic('sqrt domain error');
end;
  sqrt_guess := x / 2;
  sqrt_i := 0;
  while sqrt_i < 20 do begin
  sqrt_guess := (sqrt_guess + (x / sqrt_guess)) / 2;
  sqrt_i := sqrt_i + 1;
end;
  exit(sqrt_guess);
end;
function ln(x: real): real;
var
  ln_y: real;
  ln_y2: real;
  ln_term: real;
  ln_sum: real;
  ln_k: integer;
  ln_denom: real;
begin
  if x <= 0 then begin
  panic('ln domain error');
end;
  ln_y := (x - 1) / (x + 1);
  ln_y2 := ln_y * ln_y;
  ln_term := ln_y;
  ln_sum := 0;
  ln_k := 0;
  while ln_k < 10 do begin
  ln_denom := Double((2 * ln_k) + 1);
  ln_sum := ln_sum + (ln_term / ln_denom);
  ln_term := ln_term * ln_y2;
  ln_k := ln_k + 1;
end;
  exit(2 * ln_sum);
end;
function exp_series(x: real): real;
var
  exp_series_term: real;
  exp_series_sum: real;
  exp_series_n: integer;
begin
  exp_series_term := 1;
  exp_series_sum := 1;
  exp_series_n := 1;
  while exp_series_n < 20 do begin
  exp_series_term := (exp_series_term * x) / Double(exp_series_n);
  exp_series_sum := exp_series_sum + exp_series_term;
  exp_series_n := exp_series_n + 1;
end;
  exit(exp_series_sum);
end;
function powf(base: real; exponent: real): real;
begin
  if base <= 0 then begin
  exit(0);
end;
  exit(exp_series(exponent * ln(base)));
end;
function integrand(x: real; z: real): real;
begin
  exit(powf(x, z - 1) * exp_series(-x));
end;
function gamma_iterative(num: real): real;
var
  gamma_iterative_step: real;
  gamma_iterative_limit: real;
  gamma_iterative_x: real;
  gamma_iterative_total: real;
begin
  if num <= 0 then begin
  panic('math domain error');
end;
  gamma_iterative_step := 0.001;
  gamma_iterative_limit := 100;
  gamma_iterative_x := gamma_iterative_step;
  gamma_iterative_total := 0;
  while gamma_iterative_x < gamma_iterative_limit do begin
  gamma_iterative_total := gamma_iterative_total + (integrand(gamma_iterative_x, num) * gamma_iterative_step);
  gamma_iterative_x := gamma_iterative_x + gamma_iterative_step;
end;
  exit(gamma_iterative_total);
end;
function gamma_recursive(num: real): real;
var
  gamma_recursive_int_part: integer;
  gamma_recursive_frac: real;
begin
  if num <= 0 then begin
  panic('math domain error');
end;
  if num > 171.5 then begin
  panic('math range error');
end;
  gamma_recursive_int_part := Trunc(num);
  gamma_recursive_frac := num - Double(gamma_recursive_int_part);
  if not ((absf(gamma_recursive_frac) < 1e-06) or (absf(gamma_recursive_frac - 0.5) < 1e-06)) then begin
  panic('num must be an integer or a half-integer');
end;
  if absf(num - 0.5) < 1e-06 then begin
  exit(sqrt(PI));
end;
  if absf(num - 1) < 1e-06 then begin
  exit(1);
end;
  exit((num - 1) * gamma_recursive(num - 1));
end;
procedure main();
begin
  writeln(gamma_iterative(5));
  writeln(gamma_recursive(5));
  writeln(gamma_recursive(0.5));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
