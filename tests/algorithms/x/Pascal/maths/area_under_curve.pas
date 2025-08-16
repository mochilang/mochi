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
  i: integer;
  result_: real;
  f_var: FuncType1;
  x_start: real;
  x: real;
  x_end: real;
  steps: integer;
function abs_float(x: real): real; forward;
function trapezoidal_area(trapezoidal_area_f_var: FuncType1; x_start: real; x_end: real; steps: integer): real; forward;
function f(x: real): real; forward;
function abs_float(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end else begin
  exit(x);
end;
end;
function trapezoidal_area(trapezoidal_area_f_var: FuncType1; x_start: real; x_end: real; steps: integer): real;
var
  trapezoidal_area_step: real;
  trapezoidal_area_x1: real;
  trapezoidal_area_fx1: real;
  trapezoidal_area_area: real;
  trapezoidal_area_i: integer;
  trapezoidal_area_x2: real;
  trapezoidal_area_fx2: real;
begin
  trapezoidal_area_step := (x_end - x_start) / Double(steps);
  trapezoidal_area_x1 := x_start;
  trapezoidal_area_fx1 := f(x_start);
  trapezoidal_area_area := 0;
  trapezoidal_area_i := 0;
  while trapezoidal_area_i < steps do begin
  trapezoidal_area_x2 := trapezoidal_area_x1 + trapezoidal_area_step;
  trapezoidal_area_fx2 := f(trapezoidal_area_x2);
  trapezoidal_area_area := trapezoidal_area_area + ((abs_float(trapezoidal_area_fx2 + trapezoidal_area_fx1) * trapezoidal_area_step) / 2);
  trapezoidal_area_x1 := trapezoidal_area_x2;
  trapezoidal_area_fx1 := trapezoidal_area_fx2;
  trapezoidal_area_i := trapezoidal_area_i + 1;
end;
  exit(trapezoidal_area_area);
end;
function f(x: real): real;
begin
  exit(((x * x) * x) + (x * x));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln('f(x) = x^3 + x^2');
  writeln('The area between the curve, x = -5, x = 5 and the x axis is:');
  i := 10;
  while i <= 100000 do begin
  result_ := trapezoidal_area(f_var, -5, 5, i);
  writeln((('with ' + IntToStr(i)) + ' steps: ') + FloatToStr(result_));
  i := i * 10;
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
