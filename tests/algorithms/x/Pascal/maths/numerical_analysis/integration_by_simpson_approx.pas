{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type FuncType1 = function(arg0: real): real is nested;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  N_STEPS: integer;
  a: real;
  b: real;
  f: FuncType1;
  n: integer;
  precision: integer;
  x: real;
function floor(x: real): real; forward;
function pow10(n: integer): real; forward;
function round(x: real; n: integer): real; forward;
function simpson_integration(f: FuncType1; a: real; b: real; precision: integer): real; forward;
function square(x: real): real; forward;
function floor(x: real): real;
var
  floor_i: integer;
begin
  floor_i := Trunc(x);
  if Double(floor_i) > x then begin
  floor_i := floor_i - 1;
end;
  exit(Double(floor_i));
end;
function pow10(n: integer): real;
var
  pow10_p: real;
  pow10_i: integer;
begin
  pow10_p := 1;
  pow10_i := 0;
  while pow10_i < n do begin
  pow10_p := pow10_p * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_p);
end;
function round(x: real; n: integer): real;
var
  round_m: real;
begin
  round_m := pow10(n);
  exit(Floor((x * round_m) + 0.5) / round_m);
end;
function simpson_integration(f: FuncType1; a: real; b: real; precision: integer): real;
var
  simpson_integration_h: real;
  simpson_integration_result_: real;
  simpson_integration_i: integer;
  simpson_integration_x: real;
  simpson_integration_r: real;
begin
  if precision <= 0 then begin
  panic('precision should be positive');
end;
  simpson_integration_h := (b - a) / Double(N_STEPS);
  simpson_integration_result_ := f(a) + f(b);
  simpson_integration_i := 1;
  while simpson_integration_i < N_STEPS do begin
  simpson_integration_x := a + (simpson_integration_h * Double(simpson_integration_i));
  if (simpson_integration_i mod 2) = 1 then begin
  simpson_integration_result_ := simpson_integration_result_ + (4 * f(simpson_integration_x));
end else begin
  simpson_integration_result_ := simpson_integration_result_ + (2 * f(simpson_integration_x));
end;
  simpson_integration_i := simpson_integration_i + 1;
end;
  simpson_integration_result_ := simpson_integration_result_ * (simpson_integration_h / 3);
  simpson_integration_r := round(simpson_integration_result_, precision);
  exit(simpson_integration_r);
end;
function square(x: real): real;
begin
  exit(x * x);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  N_STEPS := 1000;
  writeln(FloatToStr(simpson_integration(@square, 1, 2, 3)));
  writeln(FloatToStr(simpson_integration(@square, 3.45, 3.2, 1)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
