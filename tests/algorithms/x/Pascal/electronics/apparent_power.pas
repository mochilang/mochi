{$mode objfpc}{$modeswitch nestedprocvars}
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
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  PI: real;
function abs(abs_x: real): real; forward;
function to_radians(to_radians_deg: real): real; forward;
function sin_taylor(sin_taylor_x: real): real; forward;
function cos_taylor(cos_taylor_x: real): real; forward;
function rect(rect_mag: real; rect_angle: real): RealArray; forward;
function multiply(multiply_a: RealArray; multiply_b: RealArray): RealArray; forward;
function apparent_power(apparent_power_voltage: real; apparent_power_current: real; apparent_power_voltage_angle: real; apparent_power_current_angle: real): RealArray; forward;
function approx_equal(approx_equal_a: RealArray; approx_equal_b: RealArray; approx_equal_eps: real): boolean; forward;
function abs(abs_x: real): real;
begin
  if abs_x < 0 then begin
  exit(-abs_x);
end;
  exit(abs_x);
end;
function to_radians(to_radians_deg: real): real;
begin
  exit((to_radians_deg * PI) / 180);
end;
function sin_taylor(sin_taylor_x: real): real;
var
  sin_taylor_term: real;
  sin_taylor_sum: real;
  sin_taylor_i: int64;
  sin_taylor_k1: real;
  sin_taylor_k2: real;
begin
  sin_taylor_term := sin_taylor_x;
  sin_taylor_sum := sin_taylor_x;
  sin_taylor_i := 1;
  while sin_taylor_i < 10 do begin
  sin_taylor_k1 := 2 * Double(sin_taylor_i);
  sin_taylor_k2 := sin_taylor_k1 + 1;
  sin_taylor_term := ((-sin_taylor_term * sin_taylor_x) * sin_taylor_x) / (sin_taylor_k1 * sin_taylor_k2);
  sin_taylor_sum := sin_taylor_sum + sin_taylor_term;
  sin_taylor_i := sin_taylor_i + 1;
end;
  exit(sin_taylor_sum);
end;
function cos_taylor(cos_taylor_x: real): real;
var
  cos_taylor_term: real;
  cos_taylor_sum: real;
  cos_taylor_i: int64;
  cos_taylor_k1: real;
  cos_taylor_k2: real;
begin
  cos_taylor_term := 1;
  cos_taylor_sum := 1;
  cos_taylor_i := 1;
  while cos_taylor_i < 10 do begin
  cos_taylor_k1 := (2 * Double(cos_taylor_i)) - 1;
  cos_taylor_k2 := 2 * Double(cos_taylor_i);
  cos_taylor_term := ((-cos_taylor_term * cos_taylor_x) * cos_taylor_x) / (cos_taylor_k1 * cos_taylor_k2);
  cos_taylor_sum := cos_taylor_sum + cos_taylor_term;
  cos_taylor_i := cos_taylor_i + 1;
end;
  exit(cos_taylor_sum);
end;
function rect(rect_mag: real; rect_angle: real): RealArray;
var
  rect_c: real;
  rect_s: real;
begin
  rect_c := cos_taylor(rect_angle);
  rect_s := sin_taylor(rect_angle);
  exit([rect_mag * rect_c, rect_mag * rect_s]);
end;
function multiply(multiply_a: RealArray; multiply_b: RealArray): RealArray;
begin
  exit([(multiply_a[0] * multiply_b[0]) - (multiply_a[1] * multiply_b[1]), (multiply_a[0] * multiply_b[1]) + (multiply_a[1] * multiply_b[0])]);
end;
function apparent_power(apparent_power_voltage: real; apparent_power_current: real; apparent_power_voltage_angle: real; apparent_power_current_angle: real): RealArray;
var
  apparent_power_vrad: real;
  apparent_power_irad: real;
  apparent_power_vrect: array of real;
  apparent_power_irect: array of real;
  apparent_power_result_: array of real;
begin
  apparent_power_vrad := to_radians(apparent_power_voltage_angle);
  apparent_power_irad := to_radians(apparent_power_current_angle);
  apparent_power_vrect := rect(apparent_power_voltage, apparent_power_vrad);
  apparent_power_irect := rect(apparent_power_current, apparent_power_irad);
  apparent_power_result_ := multiply(apparent_power_vrect, apparent_power_irect);
  exit(apparent_power_result_);
end;
function approx_equal(approx_equal_a: RealArray; approx_equal_b: RealArray; approx_equal_eps: real): boolean;
begin
  exit((abs(approx_equal_a[0] - approx_equal_b[0]) < approx_equal_eps) and (abs(approx_equal_a[1] - approx_equal_b[1]) < approx_equal_eps));
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
  writeln('');
end.
