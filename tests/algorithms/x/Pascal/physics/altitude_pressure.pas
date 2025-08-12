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
  pressure: real;
  x: real;
  exponent: real;
  base: real;
function to_float(x: integer): real; forward;
function ln(x: real): real; forward;
function exp(x: real): real; forward;
function pow_float(base: real; exponent: real): real; forward;
function get_altitude_at_pressure(pressure: real): real; forward;
function to_float(x: integer): real;
begin
  exit(x * 1);
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
  ln_denom := to_float((2 * ln_k) + 1);
  ln_sum := ln_sum + (ln_term / ln_denom);
  ln_term := ln_term * ln_y2;
  ln_k := ln_k + 1;
end;
  exit(2 * ln_sum);
end;
function exp(x: real): real;
var
  exp_term: real;
  exp_sum: real;
  exp_n: integer;
begin
  exp_term := 1;
  exp_sum := 1;
  exp_n := 1;
  while exp_n < 20 do begin
  exp_term := (exp_term * x) / to_float(exp_n);
  exp_sum := exp_sum + exp_term;
  exp_n := exp_n + 1;
end;
  exit(exp_sum);
end;
function pow_float(base: real; exponent: real): real;
begin
  exit(exp(exponent * ln(base)));
end;
function get_altitude_at_pressure(pressure: real): real;
var
  get_altitude_at_pressure_ratio: real;
begin
  if pressure > 101325 then begin
  panic('Value Higher than Pressure at Sea Level !');
end;
  if pressure < 0 then begin
  panic('Atmospheric Pressure can not be negative !');
end;
  get_altitude_at_pressure_ratio := pressure / 101325;
  exit(44330 * (1 - pow_float(get_altitude_at_pressure_ratio, 1 / 5.5255)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(FloatToStr(get_altitude_at_pressure(100000)));
  writeln(FloatToStr(get_altitude_at_pressure(101325)));
  writeln(FloatToStr(get_altitude_at_pressure(80000)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
