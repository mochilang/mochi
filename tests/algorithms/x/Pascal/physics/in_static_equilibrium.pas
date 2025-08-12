{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
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
  TWO_PI: real;
  forces1: array of RealArray;
  location1: array of RealArray;
  forces2: array of RealArray;
  location2: array of RealArray;
  forces3: array of RealArray;
  location3: array of RealArray;
  forces4: array of RealArray;
  location4: array of RealArray;
  forces: RealArrayArray;
  location: RealArrayArray;
  m: real;
  magnitude: real;
  radian_mode: boolean;
  angle: real;
  eps: real;
  x: real;
function _mod(x: real; m: real): real; forward;
function sin_approx(x: real): real; forward;
function cos_approx(x: real): real; forward;
function polar_force(magnitude: real; angle: real; radian_mode: boolean): RealArray; forward;
function abs_float(x: real): real; forward;
function in_static_equilibrium(forces: RealArrayArray; location: RealArrayArray; eps: real): boolean; forward;
function _mod(x: real; m: real): real;
begin
  exit(x - (Double(Trunc(x / m)) * m));
end;
function sin_approx(x: real): real;
var
  sin_approx_y: real;
  sin_approx_y2: real;
  sin_approx_y3: real;
  sin_approx_y5: real;
  sin_approx_y7: real;
begin
  sin_approx_y := _mod(x + PI, TWO_PI) - PI;
  sin_approx_y2 := sin_approx_y * sin_approx_y;
  sin_approx_y3 := sin_approx_y2 * sin_approx_y;
  sin_approx_y5 := sin_approx_y3 * sin_approx_y2;
  sin_approx_y7 := sin_approx_y5 * sin_approx_y2;
  exit(((sin_approx_y - (sin_approx_y3 / 6)) + (sin_approx_y5 / 120)) - (sin_approx_y7 / 5040));
end;
function cos_approx(x: real): real;
var
  cos_approx_y: real;
  cos_approx_y2: real;
  cos_approx_y4: real;
  cos_approx_y6: real;
begin
  cos_approx_y := _mod(x + PI, TWO_PI) - PI;
  cos_approx_y2 := cos_approx_y * cos_approx_y;
  cos_approx_y4 := cos_approx_y2 * cos_approx_y2;
  cos_approx_y6 := cos_approx_y4 * cos_approx_y2;
  exit(((1 - (cos_approx_y2 / 2)) + (cos_approx_y4 / 24)) - (cos_approx_y6 / 720));
end;
function polar_force(magnitude: real; angle: real; radian_mode: boolean): RealArray;
var
  polar_force_theta: real;
begin
  if radian_mode then begin
  polar_force_theta := angle;
end else begin
  polar_force_theta := (angle * PI) / 180;
end;
  exit([magnitude * cos_approx(polar_force_theta), magnitude * sin_approx(polar_force_theta)]);
end;
function abs_float(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end else begin
  exit(x);
end;
end;
function in_static_equilibrium(forces: RealArrayArray; location: RealArrayArray; eps: real): boolean;
var
  in_static_equilibrium_sum_moments: real;
  in_static_equilibrium_i: integer;
  in_static_equilibrium_n: integer;
  in_static_equilibrium_r: array of real;
  in_static_equilibrium_f: array of real;
  in_static_equilibrium_moment: real;
begin
  in_static_equilibrium_sum_moments := 0;
  in_static_equilibrium_i := 0;
  in_static_equilibrium_n := Length(forces);
  while in_static_equilibrium_i < in_static_equilibrium_n do begin
  in_static_equilibrium_r := location[in_static_equilibrium_i];
  in_static_equilibrium_f := forces[in_static_equilibrium_i];
  in_static_equilibrium_moment := (in_static_equilibrium_r[0] * in_static_equilibrium_f[1]) - (in_static_equilibrium_r[1] * in_static_equilibrium_f[0]);
  in_static_equilibrium_sum_moments := in_static_equilibrium_sum_moments + in_static_equilibrium_moment;
  in_static_equilibrium_i := in_static_equilibrium_i + 1;
end;
  exit(abs_float(in_static_equilibrium_sum_moments) < eps);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  TWO_PI := 6.283185307179586;
  forces1 := [[1, 1], [-1, 2]];
  location1 := [[1, 0], [10, 0]];
  writeln(LowerCase(BoolToStr(in_static_equilibrium(forces1, location1, 0.1), true)));
  forces2 := [polar_force(718.4, 150, false), polar_force(879.54, 45, false), polar_force(100, -90, false)];
  location2 := [[0, 0], [0, 0], [0, 0]];
  writeln(LowerCase(BoolToStr(in_static_equilibrium(forces2, location2, 0.1), true)));
  forces3 := [polar_force(30 * 9.81, 15, false), polar_force(215, 135, false), polar_force(264, 60, false)];
  location3 := [[0, 0], [0, 0], [0, 0]];
  writeln(LowerCase(BoolToStr(in_static_equilibrium(forces3, location3, 0.1), true)));
  forces4 := [[0, -2000], [0, -1200], [0, 15600], [0, -12400]];
  location4 := [[0, 0], [6, 0], [10, 0], [12, 0]];
  writeln(LowerCase(BoolToStr(in_static_equilibrium(forces4, location4, 0.1), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
