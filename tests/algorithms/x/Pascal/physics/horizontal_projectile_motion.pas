{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
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
  g: real;
  v0: real;
  angle: real;
function _mod(_mod_x: real; _mod_m: real): real; forward;
function sin(sin_x: real): real; forward;
function deg_to_rad(deg_to_rad_deg: real): real; forward;
function floor(floor_x: real): real; forward;
function pow10(pow10_n: int64): real; forward;
function round(round_x: real; round_n: int64): real; forward;
procedure check_args(check_args_init_velocity: real; check_args_angle: real); forward;
function horizontal_distance(horizontal_distance_init_velocity: real; horizontal_distance_angle: real): real; forward;
function max_height(max_height_init_velocity: real; max_height_angle: real): real; forward;
function total_time(total_time_init_velocity: real; total_time_angle: real): real; forward;
function _mod(_mod_x: real; _mod_m: real): real;
begin
  exit(_mod_x - (Double(Trunc(_mod_x / _mod_m)) * _mod_m));
end;
function sin(sin_x: real): real;
var
  sin_y: real;
  sin_y2: real;
  sin_y3: real;
  sin_y5: real;
  sin_y7: real;
begin
  sin_y := _mod(sin_x + PI, TWO_PI) - PI;
  sin_y2 := sin_y * sin_y;
  sin_y3 := sin_y2 * sin_y;
  sin_y5 := sin_y3 * sin_y2;
  sin_y7 := sin_y5 * sin_y2;
  exit(((sin_y - (sin_y3 / 6)) + (sin_y5 / 120)) - (sin_y7 / 5040));
end;
function deg_to_rad(deg_to_rad_deg: real): real;
begin
  exit((deg_to_rad_deg * PI) / 180);
end;
function floor(floor_x: real): real;
var
  floor_i: integer;
begin
  floor_i := Trunc(floor_x);
  if Double(floor_i) > floor_x then begin
  floor_i := floor_i - 1;
end;
  exit(Double(floor_i));
end;
function pow10(pow10_n: int64): real;
var
  pow10_result_: real;
  pow10_i: int64;
begin
  pow10_result_ := 1;
  pow10_i := 0;
  while pow10_i < pow10_n do begin
  pow10_result_ := pow10_result_ * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_result_);
end;
function round(round_x: real; round_n: int64): real;
var
  round_m: real;
  round_y: real;
begin
  round_m := pow10(round_n);
  round_y := Floor((round_x * round_m) + 0.5);
  exit(round_y / round_m);
end;
procedure check_args(check_args_init_velocity: real; check_args_angle: real);
begin
  if (check_args_angle > 90) or (check_args_angle < 1) then begin
  panic('Invalid angle. Range is 1-90 degrees.');
end;
  if check_args_init_velocity < 0 then begin
  panic('Invalid velocity. Should be a positive number.');
end;
end;
function horizontal_distance(horizontal_distance_init_velocity: real; horizontal_distance_angle: real): real;
var
  horizontal_distance_radians: real;
begin
  check_args(horizontal_distance_init_velocity, horizontal_distance_angle);
  horizontal_distance_radians := deg_to_rad(2 * horizontal_distance_angle);
  exit(round(((horizontal_distance_init_velocity * horizontal_distance_init_velocity) * sin(horizontal_distance_radians)) / g, 2));
end;
function max_height(max_height_init_velocity: real; max_height_angle: real): real;
var
  max_height_radians: real;
  max_height_s: real;
begin
  check_args(max_height_init_velocity, max_height_angle);
  max_height_radians := deg_to_rad(max_height_angle);
  max_height_s := sin(max_height_radians);
  exit(round((((max_height_init_velocity * max_height_init_velocity) * max_height_s) * max_height_s) / (2 * g), 2));
end;
function total_time(total_time_init_velocity: real; total_time_angle: real): real;
var
  total_time_radians: real;
begin
  check_args(total_time_init_velocity, total_time_angle);
  total_time_radians := deg_to_rad(total_time_angle);
  exit(round(((2 * total_time_init_velocity) * sin(total_time_radians)) / g, 2));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  TWO_PI := 6.283185307179586;
  g := 9.80665;
  v0 := 25;
  angle := 20;
  writeln(horizontal_distance(v0, angle));
  writeln(max_height(v0, angle));
  writeln(total_time(v0, angle));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
