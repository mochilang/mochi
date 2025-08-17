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
  lower_bound: real;
  repeats: integer;
  upper_bound: real;
  x: real;
function exp_approx(x: real): real; forward;
function f(x: real): real; forward;
function secant_method(lower_bound: real; upper_bound: real; repeats: integer): real; forward;
function exp_approx(x: real): real;
var
  exp_approx_sum: real;
  exp_approx_term: real;
  exp_approx_i: integer;
begin
  exp_approx_sum := 1;
  exp_approx_term := 1;
  exp_approx_i := 1;
  while exp_approx_i <= 20 do begin
  exp_approx_term := (exp_approx_term * x) / exp_approx_i;
  exp_approx_sum := exp_approx_sum + exp_approx_term;
  exp_approx_i := exp_approx_i + 1;
end;
  exit(exp_approx_sum);
end;
function f(x: real): real;
begin
  exit((8 * x) - (2 * exp_approx(-x)));
end;
function secant_method(lower_bound: real; upper_bound: real; repeats: integer): real;
var
  secant_method_x0: real;
  secant_method_x1: real;
  secant_method_i: integer;
  secant_method_fx1: real;
  secant_method_fx0: real;
  secant_method_new_x: real;
begin
  secant_method_x0 := lower_bound;
  secant_method_x1 := upper_bound;
  secant_method_i := 0;
  while secant_method_i < repeats do begin
  secant_method_fx1 := f(secant_method_x1);
  secant_method_fx0 := f(secant_method_x0);
  secant_method_new_x := secant_method_x1 - ((secant_method_fx1 * (secant_method_x1 - secant_method_x0)) / (secant_method_fx1 - secant_method_fx0));
  secant_method_x0 := secant_method_x1;
  secant_method_x1 := secant_method_new_x;
  secant_method_i := secant_method_i + 1;
end;
  exit(secant_method_x1);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(FloatToStr(secant_method(1, 3, 2)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
