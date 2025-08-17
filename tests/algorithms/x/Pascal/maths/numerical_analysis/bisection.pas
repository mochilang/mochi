{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type FuncType1 = function(arg0: real): real is nested;
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
  a: real;
  b: real;
  f_var: FuncType1;
  x: real;
function abs_float(x: real): real; forward;
function bisection(bisection_f_var: FuncType1; a: real; b: real): real; forward;
function f(x: real): real; forward;
procedure main(); forward;
function abs_float(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end else begin
  exit(x);
end;
end;
function bisection(bisection_f_var: FuncType1; a: real; b: real): real;
var
  bisection_start: real;
  bisection_end_: real;
  bisection_mid: real;
  bisection_fmid: real;
begin
  bisection_start := a;
  bisection_end_ := b;
  if f_var(a) = 0 then begin
  exit(a);
end;
  if f_var(b) = 0 then begin
  exit(b);
end;
  if (f_var(a) * f_var(b)) > 0 then begin
  panic('could not find root in given interval.');
end;
  bisection_mid := bisection_start + ((bisection_end_ - bisection_start) / 2);
  while abs_float(bisection_start - bisection_mid) > 1e-07 do begin
  bisection_fmid := f_var(bisection_mid);
  if bisection_fmid = 0 then begin
  exit(bisection_mid);
end;
  if (bisection_fmid * f_var(bisection_start)) < 0 then begin
  bisection_end_ := bisection_mid;
end else begin
  bisection_start := bisection_mid;
end;
  bisection_mid := bisection_start + ((bisection_end_ - bisection_start) / 2);
end;
  exit(bisection_mid);
end;
function f(x: real): real;
begin
  exit((((x * x) * x) - (2 * x)) - 5);
end;
procedure main();
begin
  writeln(FloatToStr(bisection(f_var, 1, 1000)));
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
