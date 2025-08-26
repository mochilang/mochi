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
function _to_float(x: int64): real;
begin
  _to_float := x;
end;
function to_float(x: int64): real;
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
function _mod(_mod_x: real; _mod_m: real): real; forward;
function sin_approx(sin_approx_x: real): real; forward;
function cos_approx(cos_approx_x: real): real; forward;
function polar_force(polar_force_magnitude: real; polar_force_angle: real; polar_force_radian_mode: boolean): RealArray; forward;
function abs_float(abs_float_x: real): real; forward;
function in_static_equilibrium(in_static_equilibrium_forces: RealArrayArray; in_static_equilibrium_location: RealArrayArray; in_static_equilibrium_eps: real): boolean; forward;
function _mod(_mod_x: real; _mod_m: real): real;
begin
  exit(_mod_x - (Double(Trunc(_mod_x / _mod_m)) * _mod_m));
end;
function sin_approx(sin_approx_x: real): real;
var
  sin_approx_y: real;
  sin_approx_y2: real;
  sin_approx_y3: real;
  sin_approx_y5: real;
  sin_approx_y7: real;
begin
  sin_approx_y := _mod(sin_approx_x + PI, TWO_PI) - PI;
  sin_approx_y2 := sin_approx_y * sin_approx_y;
  sin_approx_y3 := sin_approx_y2 * sin_approx_y;
  sin_approx_y5 := sin_approx_y3 * sin_approx_y2;
  sin_approx_y7 := sin_approx_y5 * sin_approx_y2;
  exit(((sin_approx_y - (sin_approx_y3 / 6)) + (sin_approx_y5 / 120)) - (sin_approx_y7 / 5040));
end;
function cos_approx(cos_approx_x: real): real;
var
  cos_approx_y: real;
  cos_approx_y2: real;
  cos_approx_y4: real;
  cos_approx_y6: real;
begin
  cos_approx_y := _mod(cos_approx_x + PI, TWO_PI) - PI;
  cos_approx_y2 := cos_approx_y * cos_approx_y;
  cos_approx_y4 := cos_approx_y2 * cos_approx_y2;
  cos_approx_y6 := cos_approx_y4 * cos_approx_y2;
  exit(((1 - (cos_approx_y2 / 2)) + (cos_approx_y4 / 24)) - (cos_approx_y6 / 720));
end;
function polar_force(polar_force_magnitude: real; polar_force_angle: real; polar_force_radian_mode: boolean): RealArray;
var
  polar_force_theta: real;
begin
  if polar_force_radian_mode then begin
  polar_force_theta := polar_force_angle;
end else begin
  polar_force_theta := (polar_force_angle * PI) / 180;
end;
  exit([polar_force_magnitude * cos_approx(polar_force_theta), polar_force_magnitude * sin_approx(polar_force_theta)]);
end;
function abs_float(abs_float_x: real): real;
begin
  if abs_float_x < 0 then begin
  exit(-abs_float_x);
end else begin
  exit(abs_float_x);
end;
end;
function in_static_equilibrium(in_static_equilibrium_forces: RealArrayArray; in_static_equilibrium_location: RealArrayArray; in_static_equilibrium_eps: real): boolean;
var
  in_static_equilibrium_sum_moments: real;
  in_static_equilibrium_i: int64;
  in_static_equilibrium_n: integer;
  in_static_equilibrium_r: array of real;
  in_static_equilibrium_f: array of real;
  in_static_equilibrium_moment: real;
begin
  in_static_equilibrium_sum_moments := 0;
  in_static_equilibrium_i := 0;
  in_static_equilibrium_n := Length(in_static_equilibrium_forces);
  while in_static_equilibrium_i < in_static_equilibrium_n do begin
  in_static_equilibrium_r := in_static_equilibrium_location[in_static_equilibrium_i];
  in_static_equilibrium_f := in_static_equilibrium_forces[in_static_equilibrium_i];
  in_static_equilibrium_moment := (in_static_equilibrium_r[0] * in_static_equilibrium_f[1]) - (in_static_equilibrium_r[1] * in_static_equilibrium_f[0]);
  in_static_equilibrium_sum_moments := in_static_equilibrium_sum_moments + in_static_equilibrium_moment;
  in_static_equilibrium_i := in_static_equilibrium_i + 1;
end;
  exit(abs_float(in_static_equilibrium_sum_moments) < in_static_equilibrium_eps);
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
  writeln('');
end.
