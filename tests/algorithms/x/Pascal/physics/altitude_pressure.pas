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
function to_float(to_float_x: int64): real; forward;
function ln_(ln__x: real): real; forward;
function exp_(exp__x: real): real; forward;
function pow_float(pow_float_base: real; pow_float_exponent: real): real; forward;
function get_altitude_at_pressure(get_altitude_at_pressure_pressure: real): real; forward;
function to_float(to_float_x: int64): real;
begin
  exit(to_float_x * 1);
end;
function ln_(ln__x: real): real;
var
  ln__y: real;
  ln__y2: real;
  ln__term: real;
  ln__sum: real;
  ln__k: int64;
  ln__denom: real;
begin
  if ln__x <= 0 then begin
  panic('ln domain error');
end;
  ln__y := (ln__x - 1) / (ln__x + 1);
  ln__y2 := ln__y * ln__y;
  ln__term := ln__y;
  ln__sum := 0;
  ln__k := 0;
  while ln__k < 10 do begin
  ln__denom := to_float((2 * ln__k) + 1);
  ln__sum := ln__sum + (ln__term / ln__denom);
  ln__term := ln__term * ln__y2;
  ln__k := ln__k + 1;
end;
  exit(2 * ln__sum);
end;
function exp_(exp__x: real): real;
var
  exp__term: real;
  exp__sum: real;
  exp__n: int64;
begin
  exp__term := 1;
  exp__sum := 1;
  exp__n := 1;
  while exp__n < 20 do begin
  exp__term := (exp__term * exp__x) / to_float(exp__n);
  exp__sum := exp__sum + exp__term;
  exp__n := exp__n + 1;
end;
  exit(exp__sum);
end;
function pow_float(pow_float_base: real; pow_float_exponent: real): real;
begin
  exit(exp(pow_float_exponent * ln(pow_float_base)));
end;
function get_altitude_at_pressure(get_altitude_at_pressure_pressure: real): real;
var
  get_altitude_at_pressure_ratio: real;
begin
  if get_altitude_at_pressure_pressure > 101325 then begin
  panic('Value Higher than Pressure at Sea Level !');
end;
  if get_altitude_at_pressure_pressure < 0 then begin
  panic('Atmospheric Pressure can not be negative !');
end;
  get_altitude_at_pressure_ratio := get_altitude_at_pressure_pressure / 101325;
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
  writeln('');
end.
