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
  G: real;
  C: real;
  PI: real;
  x: real;
  target_body_mass: real;
  capture_radius: real;
  n: integer;
  target_body_radius: real;
  projectile_velocity: real;
function pow10(n: integer): real; forward;
function sqrt(x: real): real; forward;
function abs(x: real): real; forward;
function capture_radii(target_body_radius: real; target_body_mass: real; projectile_velocity: real): real; forward;
function capture_area(capture_radius: real): real; forward;
procedure run_tests(); forward;
procedure main(); forward;
function pow10(n: integer): real;
var
  pow10_result_: real;
  pow10_i: integer;
begin
  pow10_result_ := 1;
  pow10_i := 0;
  while pow10_i < n do begin
  pow10_result_ := pow10_result_ * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_result_);
end;
function sqrt(x: real): real;
var
  sqrt_guess: real;
  sqrt_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrt_guess := x;
  sqrt_i := 0;
  while sqrt_i < 20 do begin
  sqrt_guess := (sqrt_guess + (x / sqrt_guess)) / 2;
  sqrt_i := sqrt_i + 1;
end;
  exit(sqrt_guess);
end;
function abs(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function capture_radii(target_body_radius: real; target_body_mass: real; projectile_velocity: real): real;
var
  capture_radii_escape_velocity_squared: real;
  capture_radii_denom: real;
  capture_radii_capture_radius: real;
begin
  if target_body_mass < 0 then begin
  panic('Mass cannot be less than 0');
end;
  if target_body_radius < 0 then begin
  panic('Radius cannot be less than 0');
end;
  if projectile_velocity > C then begin
  panic('Cannot go beyond speed of light');
end;
  capture_radii_escape_velocity_squared := ((2 * G) * target_body_mass) / target_body_radius;
  capture_radii_denom := projectile_velocity * projectile_velocity;
  capture_radii_capture_radius := target_body_radius * sqrt(1 + (capture_radii_escape_velocity_squared / capture_radii_denom));
  exit(capture_radii_capture_radius);
end;
function capture_area(capture_radius: real): real;
var
  capture_area_sigma: real;
begin
  if capture_radius < 0 then begin
  panic('Cannot have a capture radius less than 0');
end;
  capture_area_sigma := (PI * capture_radius) * capture_radius;
  exit(capture_area_sigma);
end;
procedure run_tests();
var
  run_tests_r: real;
  run_tests_a: real;
begin
  run_tests_r := capture_radii(6.957 * pow10(8), 1.99 * pow10(30), 25000);
  if abs(run_tests_r - (1.720959069143714 * pow10(10))) > 1 then begin
  panic('capture_radii failed');
end;
  run_tests_a := capture_area(run_tests_r);
  if abs(run_tests_a - (9.304455331801812 * pow10(20))) > 1 then begin
  panic('capture_area failed');
end;
end;
procedure main();
var
  main_r: real;
begin
  run_tests();
  main_r := capture_radii(6.957 * pow10(8), 1.99 * pow10(30), 25000);
  writeln(FloatToStr(main_r));
  writeln(FloatToStr(capture_area(main_r)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  G := 6.6743e-11;
  C := 2.99792458e+08;
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
