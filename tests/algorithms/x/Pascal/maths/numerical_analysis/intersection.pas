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
  function_: FuncType1;
  x: real;
  x0: real;
  x1: real;
function abs_float(x: real): real; forward;
function intersection(intersection_function_: FuncType1; x0: real; x1: real): real; forward;
function f(x: real): real; forward;
procedure main(); forward;
function abs_float(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function intersection(intersection_function_: FuncType1; x0: real; x1: real): real;
var
  intersection_x_n: real;
  intersection_x_n1: real;
  intersection_numerator: real;
  intersection_denominator: real;
  intersection_x_n2: real;
begin
  intersection_x_n := x0;
  intersection_x_n1 := x1;
  while true do begin
  if (intersection_x_n = intersection_x_n1) or (function_(intersection_x_n1) = function_(intersection_x_n)) then begin
  panic('float division by zero, could not find root');
end;
  intersection_numerator := function_(intersection_x_n1);
  intersection_denominator := (function_(intersection_x_n1) - function_(intersection_x_n)) / (intersection_x_n1 - intersection_x_n);
  intersection_x_n2 := intersection_x_n1 - (intersection_numerator / intersection_denominator);
  if abs_float(intersection_x_n2 - intersection_x_n1) < 1e-05 then begin
  exit(intersection_x_n2);
end;
  intersection_x_n := intersection_x_n1;
  intersection_x_n1 := intersection_x_n2;
end;
end;
function f(x: real): real;
begin
  exit((((x * x) * x) - (2 * x)) - 5);
end;
procedure main();
begin
  writeln(FloatToStr(intersection(@f, 3, 3.5)));
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
