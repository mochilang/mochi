{$mode objfpc}
program Main;
uses SysUtils;
type RealArray = array of real;
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
  a: RealArray;
  b: RealArray;
  voltage: real;
  eps: real;
  deg: real;
  current: real;
  x: real;
  current_angle: real;
  angle: real;
  mag: real;
  voltage_angle: real;
function abs(x: real): real; forward;
function to_radians(deg: real): real; forward;
function sin_taylor(x: real): real; forward;
function cos_taylor(x: real): real; forward;
function rect(mag: real; angle: real): RealArray; forward;
function multiply(a: RealArray; b: RealArray): RealArray; forward;
function apparent_power(voltage: real; current: real; voltage_angle: real; current_angle: real): RealArray; forward;
function approx_equal(a: RealArray; b: RealArray; eps: real): boolean; forward;
function abs(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function to_radians(deg: real): real;
begin
  exit((deg * PI) / 180);
end;
function sin_taylor(x: real): real;
var
  sin_taylor_term: real;
  sin_taylor_sum: real;
  sin_taylor_i: integer;
  sin_taylor_k1: real;
  sin_taylor_k2: real;
begin
  sin_taylor_term := x;
  sin_taylor_sum := x;
  sin_taylor_i := 1;
  while sin_taylor_i < 10 do begin
  sin_taylor_k1 := 2 * Double(sin_taylor_i);
  sin_taylor_k2 := sin_taylor_k1 + 1;
  sin_taylor_term := ((-sin_taylor_term * x) * x) / (sin_taylor_k1 * sin_taylor_k2);
  sin_taylor_sum := sin_taylor_sum + sin_taylor_term;
  sin_taylor_i := sin_taylor_i + 1;
end;
  exit(sin_taylor_sum);
end;
function cos_taylor(x: real): real;
var
  cos_taylor_term: real;
  cos_taylor_sum: real;
  cos_taylor_i: integer;
  cos_taylor_k1: real;
  cos_taylor_k2: real;
begin
  cos_taylor_term := 1;
  cos_taylor_sum := 1;
  cos_taylor_i := 1;
  while cos_taylor_i < 10 do begin
  cos_taylor_k1 := (2 * Double(cos_taylor_i)) - 1;
  cos_taylor_k2 := 2 * Double(cos_taylor_i);
  cos_taylor_term := ((-cos_taylor_term * x) * x) / (cos_taylor_k1 * cos_taylor_k2);
  cos_taylor_sum := cos_taylor_sum + cos_taylor_term;
  cos_taylor_i := cos_taylor_i + 1;
end;
  exit(cos_taylor_sum);
end;
function rect(mag: real; angle: real): RealArray;
var
  rect_c: real;
  rect_s: real;
begin
  rect_c := cos_taylor(angle);
  rect_s := sin_taylor(angle);
  exit([mag * rect_c, mag * rect_s]);
end;
function multiply(a: RealArray; b: RealArray): RealArray;
begin
  exit([(a[0] * b[0]) - (a[1] * b[1]), (a[0] * b[1]) + (a[1] * b[0])]);
end;
function apparent_power(voltage: real; current: real; voltage_angle: real; current_angle: real): RealArray;
var
  apparent_power_vrad: real;
  apparent_power_irad: real;
  apparent_power_vrect: array of real;
  apparent_power_irect: array of real;
  apparent_power_result_: array of real;
begin
  apparent_power_vrad := to_radians(voltage_angle);
  apparent_power_irad := to_radians(current_angle);
  apparent_power_vrect := rect(voltage, apparent_power_vrad);
  apparent_power_irect := rect(current, apparent_power_irad);
  apparent_power_result_ := multiply(apparent_power_vrect, apparent_power_irect);
  exit(apparent_power_result_);
end;
function approx_equal(a: RealArray; b: RealArray; eps: real): boolean;
begin
  exit((abs(a[0] - b[0]) < eps) and (abs(a[1] - b[1]) < eps));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
