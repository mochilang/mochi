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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  y: real;
  ode_func: FuncType1;
  x: real;
  x0: real;
  x_end: real;
  y0: real;
  step: real;
function ceil_float(x: real): integer; forward;
function exp_approx(x: real): real; forward;
function euler_modified(ode_func: FuncType1; y0: real; x0: real; step: real; x_end: real): RealArray; forward;
function f1(x: real; y: real): real; forward;
function f2(x: real; y: real): real; forward;
procedure main(); forward;
function ceil_float(x: real): integer;
var
  ceil_float_i: integer;
begin
  ceil_float_i := Trunc(x);
  if x > Double(ceil_float_i) then begin
  exit(ceil_float_i + 1);
end;
  exit(ceil_float_i);
end;
function exp_approx(x: real): real;
var
  exp_approx_term: real;
  exp_approx_sum: real;
  exp_approx_n: integer;
begin
  exp_approx_term := 1;
  exp_approx_sum := 1;
  exp_approx_n := 1;
  while exp_approx_n < 20 do begin
  exp_approx_term := (exp_approx_term * x) / Double(exp_approx_n);
  exp_approx_sum := exp_approx_sum + exp_approx_term;
  exp_approx_n := exp_approx_n + 1;
end;
  exit(exp_approx_sum);
end;
function euler_modified(ode_func: FuncType1; y0: real; x0: real; step: real; x_end: real): RealArray;
var
  euler_modified_n: integer;
  euler_modified_y: array of real;
  euler_modified_x: real;
  euler_modified_k: integer;
  euler_modified_y_predict: real;
  euler_modified_slope1: real;
  euler_modified_slope2: real;
  euler_modified_y_next: real;
begin
  euler_modified_n := ceil_float((x_end - x0) / step);
  euler_modified_y := [y0];
  euler_modified_x := x0;
  euler_modified_k := 0;
  while euler_modified_k < euler_modified_n do begin
  euler_modified_y_predict := euler_modified_y[euler_modified_k] + (step * ode_func(euler_modified_x, euler_modified_y[euler_modified_k]));
  euler_modified_slope1 := ode_func(euler_modified_x, euler_modified_y[euler_modified_k]);
  euler_modified_slope2 := ode_func(euler_modified_x + step, euler_modified_y_predict);
  euler_modified_y_next := euler_modified_y[euler_modified_k] + ((step / 2) * (euler_modified_slope1 + euler_modified_slope2));
  euler_modified_y := concat(euler_modified_y, [euler_modified_y_next]);
  euler_modified_x := euler_modified_x + step;
  euler_modified_k := euler_modified_k + 1;
end;
  exit(euler_modified_y);
end;
function f1(x: real; y: real): real;
begin
  exit(((-2 * x) * y) * y);
end;
function f2(x: real; y: real): real;
begin
  exit((-2 * y) + (((x * x) * x) * exp_approx(-2 * x)));
end;
procedure main();
var
  main_y1: RealArray;
  main_y2: RealArray;
begin
  main_y1 := euler_modified(@f1, 1, 0, 0.2, 1);
  writeln(main_y1[Length(main_y1) - 1]);
  main_y2 := euler_modified(@f2, 1, 0, 0.1, 0.3);
  writeln(main_y2[Length(main_y2) - 1]);
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
