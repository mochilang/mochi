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
  PI: real;
  accuracy: integer;
  angle_in_degrees: real;
  deg: real;
  n: integer;
  rounded_values_count: integer;
  x: real;
function abs(x: real): real; forward;
function floor(x: real): real; forward;
function pow(x: real; n: integer): real; forward;
function factorial(n: integer): real; forward;
function radians(deg: real): real; forward;
function taylor_sin(angle_in_degrees: real; accuracy: integer; rounded_values_count: integer): real; forward;
procedure test_sin(); forward;
procedure main(); forward;
function abs(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
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
function pow(x: real; n: integer): real;
var
  pow_result_: real;
  pow_i: integer;
begin
  pow_result_ := 1;
  pow_i := 0;
  while pow_i < n do begin
  pow_result_ := pow_result_ * x;
  pow_i := pow_i + 1;
end;
  exit(pow_result_);
end;
function factorial(n: integer): real;
var
  factorial_result_: real;
  factorial_i: integer;
begin
  factorial_result_ := 1;
  factorial_i := 2;
  while factorial_i <= n do begin
  factorial_result_ := factorial_result_ * Double(factorial_i);
  factorial_i := factorial_i + 1;
end;
  exit(factorial_result_);
end;
function radians(deg: real): real;
begin
  exit((deg * PI) / 180);
end;
function taylor_sin(angle_in_degrees: real; accuracy: integer; rounded_values_count: integer): real;
var
  taylor_sin_k: real;
  taylor_sin_angle: real;
  taylor_sin_angle_in_radians: real;
  taylor_sin_result_: real;
  taylor_sin_a: integer;
  taylor_sin_sign: real;
  taylor_sin_i: integer;
begin
  taylor_sin_k := Floor(angle_in_degrees / 360);
  taylor_sin_angle := angle_in_degrees - (taylor_sin_k * 360);
  taylor_sin_angle_in_radians := radians(taylor_sin_angle);
  taylor_sin_result_ := taylor_sin_angle_in_radians;
  taylor_sin_a := 3;
  taylor_sin_sign := -1;
  taylor_sin_i := 0;
  while taylor_sin_i < accuracy do begin
  taylor_sin_result_ := taylor_sin_result_ + ((taylor_sin_sign * pow(taylor_sin_angle_in_radians, taylor_sin_a)) / factorial(taylor_sin_a));
  taylor_sin_sign := -taylor_sin_sign;
  taylor_sin_a := taylor_sin_a + 2;
  taylor_sin_i := taylor_sin_i + 1;
end;
  exit(taylor_sin_result_);
end;
procedure test_sin();
var
  test_sin_eps: real;
begin
  test_sin_eps := 1e-07;
  if abs(taylor_sin(0, 18, 10) - 0) > test_sin_eps then begin
  panic('sin(0) failed');
end;
  if abs(taylor_sin(90, 18, 10) - 1) > test_sin_eps then begin
  panic('sin(90) failed');
end;
  if abs(taylor_sin(180, 18, 10) - 0) > test_sin_eps then begin
  panic('sin(180) failed');
end;
  if abs(taylor_sin(270, 18, 10) - -1) > test_sin_eps then begin
  panic('sin(270) failed');
end;
end;
procedure main();
var
  main_res: real;
begin
  test_sin();
  main_res := taylor_sin(64, 18, 10);
  writeln(main_res);
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
