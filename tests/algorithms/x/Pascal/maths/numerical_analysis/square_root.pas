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
  r1: real;
  r2: real;
  r3: real;
  a: real;
  max_iter: integer;
  tolerance: real;
  x: real;
function fx(x: real; a: real): real; forward;
function fx_derivative(x: real): real; forward;
function get_initial_point(a: real): real; forward;
function abs_float(x: real): real; forward;
function square_root_iterative(a: real; max_iter: integer; tolerance: real): real; forward;
function fx(x: real; a: real): real;
begin
  exit((x * x) - a);
end;
function fx_derivative(x: real): real;
begin
  exit(2 * x);
end;
function get_initial_point(a: real): real;
var
  get_initial_point_start: real;
begin
  get_initial_point_start := 2;
  while get_initial_point_start <= a do begin
  get_initial_point_start := get_initial_point_start * get_initial_point_start;
end;
  exit(get_initial_point_start);
end;
function abs_float(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function square_root_iterative(a: real; max_iter: integer; tolerance: real): real;
var
  square_root_iterative_value: real;
  square_root_iterative_i: integer;
  square_root_iterative_prev_value: real;
begin
  if a < 0 then begin
  panic('math domain error');
end;
  square_root_iterative_value := get_initial_point(a);
  square_root_iterative_i := 0;
  while square_root_iterative_i < max_iter do begin
  square_root_iterative_prev_value := square_root_iterative_value;
  square_root_iterative_value := square_root_iterative_value - (fx(square_root_iterative_value, a) / fx_derivative(square_root_iterative_value));
  if abs_float(square_root_iterative_prev_value - square_root_iterative_value) < tolerance then begin
  exit(square_root_iterative_value);
end;
  square_root_iterative_i := square_root_iterative_i + 1;
end;
  exit(square_root_iterative_value);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  r1 := square_root_iterative(4, 9999, 1e-14);
  writeln(FloatToStr(r1));
  r2 := square_root_iterative(3.2, 9999, 1e-14);
  writeln(FloatToStr(r2));
  r3 := square_root_iterative(140, 9999, 1e-14);
  writeln(FloatToStr(r3));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
