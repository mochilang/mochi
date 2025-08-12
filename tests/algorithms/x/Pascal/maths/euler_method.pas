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
  ys: RealArray;
  last: real;
  ys_14: RealArray;
  x: real;
  y0: real;
  x0: real;
  step_size: real;
  ode_func: FuncType1;
  x_end: real;
  a: real;
function ceil_int(x: real): integer; forward;
function explicit_euler(ode_func: FuncType1; y0: real; x0: real; step_size: real; x_end: real): RealArray; forward;
function abs_float(a: real): real; forward;
function anon3(x: real; y: real): real; forward;
procedure test_explicit_euler(); forward;
function anon5(x: real; y: real): real; forward;
procedure main(); forward;
function ceil_int(x: real): integer;
var
  ceil_int_n: integer;
begin
  ceil_int_n := Trunc(x);
  if Double(ceil_int_n) < x then begin
  ceil_int_n := ceil_int_n + 1;
end;
  exit(ceil_int_n);
end;
function explicit_euler(ode_func: FuncType1; y0: real; x0: real; step_size: real; x_end: real): RealArray;
var
  explicit_euler_n: integer;
  explicit_euler_y: array of real;
  explicit_euler_i: integer;
  explicit_euler_x: real;
  explicit_euler_k: integer;
begin
  explicit_euler_n := ceil_int((x_end - x0) / step_size);
  explicit_euler_y := [];
  explicit_euler_i := 0;
  while explicit_euler_i <= explicit_euler_n do begin
  explicit_euler_y := concat(explicit_euler_y, [0]);
  explicit_euler_i := explicit_euler_i + 1;
end;
  explicit_euler_y[0] := y0;
  explicit_euler_x := x0;
  explicit_euler_k := 0;
  while explicit_euler_k < explicit_euler_n do begin
  explicit_euler_y[explicit_euler_k + 1] := explicit_euler_y[explicit_euler_k] + (step_size * ode_func(explicit_euler_x, explicit_euler_y[explicit_euler_k]));
  explicit_euler_x := explicit_euler_x + step_size;
  explicit_euler_k := explicit_euler_k + 1;
end;
  exit(explicit_euler_y);
end;
function abs_float(a: real): real;
begin
  if a < 0 then begin
  exit(-a);
end;
  exit(a);
end;
function anon3(x: real; y: real): real;
begin
  exit(y);
end;
procedure test_explicit_euler();
var
  test_explicit_euler_f: FuncType1;
begin
  test_explicit_euler_f := @anon3;
  ys := explicit_euler(test_explicit_euler_f, 1, 0, 0.01, 5);
  last := ys[Length(ys) - 1];
  if abs_float(last - 144.77277243257308) > 0.001 then begin
  panic('explicit_euler failed');
end;
end;
function anon5(x: real; y: real): real;
begin
  exit(y);
end;
procedure main();
var
  main_f: FuncType1;
begin
  test_explicit_euler();
  main_f := @anon5;
  ys_14 := explicit_euler(main_f, 1, 0, 0.01, 5);
  writeln(ys_14[Length(ys_14) - 1]);
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
