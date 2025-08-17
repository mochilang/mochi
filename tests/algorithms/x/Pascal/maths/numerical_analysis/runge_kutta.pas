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
  f: FuncType1;
  h: real;
  x0: real;
  x_end: real;
  y0: real;
function runge_kutta(f: FuncType1; y0: real; x0: real; h: real; x_end: real): RealArray; forward;
procedure test_runge_kutta(); forward;
procedure main(); forward;
function runge_kutta(f: FuncType1; y0: real; x0: real; h: real; x_end: real): RealArray;
var
  runge_kutta_span: real;
  runge_kutta_n: integer;
  runge_kutta_y: array of real;
  runge_kutta_i: integer;
  runge_kutta_x: real;
  runge_kutta_k: integer;
  runge_kutta_k1: real;
  runge_kutta_k2: real;
  runge_kutta_k3: real;
  runge_kutta_k4: real;
begin
  runge_kutta_span := (x_end - x0) / h;
  runge_kutta_n := Trunc(runge_kutta_span);
  if Double(runge_kutta_n) < runge_kutta_span then begin
  runge_kutta_n := runge_kutta_n + 1;
end;
  runge_kutta_y := [];
  runge_kutta_i := 0;
  while runge_kutta_i < (runge_kutta_n + 1) do begin
  runge_kutta_y := concat(runge_kutta_y, [0]);
  runge_kutta_i := runge_kutta_i + 1;
end;
  runge_kutta_y[0] := y0;
  runge_kutta_x := x0;
  runge_kutta_k := 0;
  while runge_kutta_k < runge_kutta_n do begin
  runge_kutta_k1 := f(runge_kutta_x, runge_kutta_y[runge_kutta_k]);
  runge_kutta_k2 := f(runge_kutta_x + (0.5 * h), runge_kutta_y[runge_kutta_k] + ((0.5 * h) * runge_kutta_k1));
  runge_kutta_k3 := f(runge_kutta_x + (0.5 * h), runge_kutta_y[runge_kutta_k] + ((0.5 * h) * runge_kutta_k2));
  runge_kutta_k4 := f(runge_kutta_x + h, runge_kutta_y[runge_kutta_k] + (h * runge_kutta_k3));
  runge_kutta_y[runge_kutta_k + 1] := runge_kutta_y[runge_kutta_k] + (((1 / 6) * h) * (((runge_kutta_k1 + (2 * runge_kutta_k2)) + (2 * runge_kutta_k3)) + runge_kutta_k4));
  runge_kutta_x := runge_kutta_x + h;
  runge_kutta_k := runge_kutta_k + 1;
end;
  exit(runge_kutta_y);
end;
procedure test_runge_kutta();
var
  test_runge_kutta_result_: RealArray;
  test_runge_kutta_last: real;
  test_runge_kutta_expected: real;
  test_runge_kutta_diff: real;
  test_runge_kutta_x: real;
  test_runge_kutta_y: real;
  function f(f_test_runge_kutta_x: real; f_test_runge_kutta_y: real): real;
begin
  exit(test_runge_kutta_y);
end;
begin
  test_runge_kutta_result_ := runge_kutta(@f, 1, 0, 0.01, 5);
  test_runge_kutta_last := test_runge_kutta_result_[Length(test_runge_kutta_result_) - 1];
  test_runge_kutta_expected := 148.41315904125113;
  test_runge_kutta_diff := test_runge_kutta_last - test_runge_kutta_expected;
  if test_runge_kutta_diff < 0 then begin
  test_runge_kutta_diff := -test_runge_kutta_diff;
end;
  if test_runge_kutta_diff > 1e-06 then begin
  panic('runge_kutta failed');
end;
end;
procedure main();
var
  main_r: RealArray;
  main_x: real;
  main_y: real;
  function f(f_main_x: real; f_main_y: real): real;
begin
  exit(main_y);
end;
begin
  test_runge_kutta();
  main_r := runge_kutta(@f, 1, 0, 0.1, 1);
  writeln(FloatToStr(main_r[Length(main_r) - 1]));
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
