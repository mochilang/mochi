{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type FuncType1 = function(arg0: real; arg1: real): real is nested;
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
  func: FuncType1;
  step_size: real;
  x_final: real;
  x_initial: real;
  y_initial: real;
function runge_kutta_fehlberg_45(func: FuncType1; x_initial: real; y_initial: real; step_size: real; x_final: real): RealArray; forward;
procedure main(); forward;
function runge_kutta_fehlberg_45(func: FuncType1; x_initial: real; y_initial: real; step_size: real; x_final: real): RealArray;
var
  runge_kutta_fehlberg_45_n: integer;
  runge_kutta_fehlberg_45_ys: array of real;
  runge_kutta_fehlberg_45_x: real;
  runge_kutta_fehlberg_45_y: real;
  runge_kutta_fehlberg_45_i: integer;
  runge_kutta_fehlberg_45_k1: real;
  runge_kutta_fehlberg_45_k2: real;
  runge_kutta_fehlberg_45_k3: real;
  runge_kutta_fehlberg_45_k4: real;
  runge_kutta_fehlberg_45_k5: real;
  runge_kutta_fehlberg_45_k6: real;
begin
  if x_initial >= x_final then begin
  panic('The final value of x must be greater than initial value of x.');
end;
  if step_size <= 0 then begin
  panic('Step size must be positive.');
end;
  runge_kutta_fehlberg_45_n := Trunc((x_final - x_initial) / step_size);
  runge_kutta_fehlberg_45_ys := [];
  runge_kutta_fehlberg_45_x := x_initial;
  runge_kutta_fehlberg_45_y := y_initial;
  runge_kutta_fehlberg_45_ys := concat(runge_kutta_fehlberg_45_ys, [runge_kutta_fehlberg_45_y]);
  runge_kutta_fehlberg_45_i := 0;
  while runge_kutta_fehlberg_45_i < runge_kutta_fehlberg_45_n do begin
  runge_kutta_fehlberg_45_k1 := step_size * func(runge_kutta_fehlberg_45_x, runge_kutta_fehlberg_45_y);
  runge_kutta_fehlberg_45_k2 := step_size * func(runge_kutta_fehlberg_45_x + (step_size / 4), runge_kutta_fehlberg_45_y + (runge_kutta_fehlberg_45_k1 / 4));
  runge_kutta_fehlberg_45_k3 := step_size * func(runge_kutta_fehlberg_45_x + ((3 / 8) * step_size), (runge_kutta_fehlberg_45_y + ((3 / 32) * runge_kutta_fehlberg_45_k1)) + ((9 / 32) * runge_kutta_fehlberg_45_k2));
  runge_kutta_fehlberg_45_k4 := step_size * func(runge_kutta_fehlberg_45_x + ((12 / 13) * step_size), ((runge_kutta_fehlberg_45_y + ((1932 / 2197) * runge_kutta_fehlberg_45_k1)) - ((7200 / 2197) * runge_kutta_fehlberg_45_k2)) + ((7296 / 2197) * runge_kutta_fehlberg_45_k3));
  runge_kutta_fehlberg_45_k5 := step_size * func(runge_kutta_fehlberg_45_x + step_size, (((runge_kutta_fehlberg_45_y + ((439 / 216) * runge_kutta_fehlberg_45_k1)) - (8 * runge_kutta_fehlberg_45_k2)) + ((3680 / 513) * runge_kutta_fehlberg_45_k3)) - ((845 / 4104) * runge_kutta_fehlberg_45_k4));
  runge_kutta_fehlberg_45_k6 := step_size * func(runge_kutta_fehlberg_45_x + (step_size / 2), ((((runge_kutta_fehlberg_45_y - ((8 / 27) * runge_kutta_fehlberg_45_k1)) + (2 * runge_kutta_fehlberg_45_k2)) - ((3544 / 2565) * runge_kutta_fehlberg_45_k3)) + ((1859 / 4104) * runge_kutta_fehlberg_45_k4)) - ((11 / 40) * runge_kutta_fehlberg_45_k5));
  runge_kutta_fehlberg_45_y := ((((runge_kutta_fehlberg_45_y + ((16 / 135) * runge_kutta_fehlberg_45_k1)) + ((6656 / 12825) * runge_kutta_fehlberg_45_k3)) + ((28561 / 56430) * runge_kutta_fehlberg_45_k4)) - ((9 / 50) * runge_kutta_fehlberg_45_k5)) + ((2 / 55) * runge_kutta_fehlberg_45_k6);
  runge_kutta_fehlberg_45_x := runge_kutta_fehlberg_45_x + step_size;
  runge_kutta_fehlberg_45_ys := concat(runge_kutta_fehlberg_45_ys, [runge_kutta_fehlberg_45_y]);
  runge_kutta_fehlberg_45_i := runge_kutta_fehlberg_45_i + 1;
end;
  exit(runge_kutta_fehlberg_45_ys);
end;
procedure main();
var
  main_y1: RealArray;
  main_y2: RealArray;
  main_x: real;
  main_y: real;
  function f1(f1_main_x: real; f1_main_y: real): real;
begin
  exit(1 + (main_y * main_y));
end;
  function f2(f1_main_x: real; f1_main_y: real): real;
begin
  exit(main_x);
end;
begin
  main_y1 := runge_kutta_fehlberg_45(@f1, 0, 0, 0.2, 1);
  writeln(main_y1[1]);
  main_y2 := runge_kutta_fehlberg_45(@f2, -1, 0, 0.2, 0);
  writeln(main_y2[1]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
