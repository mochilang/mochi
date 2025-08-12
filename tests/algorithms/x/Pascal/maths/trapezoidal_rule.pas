{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
  a: real;
  b: real;
  steps: real;
  boundary: array of real;
  y: real;
  x: real;
  h: real;
function f(x: real): real; forward;
function make_points(a: real; b: real; h: real): RealArray; forward;
function trapezoidal_rule(boundary: RealArray; steps: real): real; forward;
function f(x: real): real;
begin
  exit(x * x);
end;
function make_points(a: real; b: real; h: real): RealArray;
var
  make_points_xs: array of real;
  make_points_x: real;
begin
  make_points_xs := [];
  make_points_x := a + h;
  while make_points_x <= (b - h) do begin
  make_points_xs := concat(make_points_xs, [make_points_x]);
  make_points_x := make_points_x + h;
end;
  exit(make_points_xs);
end;
function trapezoidal_rule(boundary: RealArray; steps: real): real;
var
  trapezoidal_rule_h: real;
  trapezoidal_rule_a: real;
  trapezoidal_rule_b: real;
  trapezoidal_rule_xs: array of real;
  trapezoidal_rule_y: real;
  trapezoidal_rule_i: integer;
begin
  trapezoidal_rule_h := (boundary[1] - boundary[0]) / steps;
  trapezoidal_rule_a := boundary[0];
  trapezoidal_rule_b := boundary[1];
  trapezoidal_rule_xs := make_points(trapezoidal_rule_a, trapezoidal_rule_b, trapezoidal_rule_h);
  trapezoidal_rule_y := (trapezoidal_rule_h / 2) * f(trapezoidal_rule_a);
  trapezoidal_rule_i := 0;
  while trapezoidal_rule_i < Length(trapezoidal_rule_xs) do begin
  trapezoidal_rule_y := trapezoidal_rule_y + (trapezoidal_rule_h * f(trapezoidal_rule_xs[trapezoidal_rule_i]));
  trapezoidal_rule_i := trapezoidal_rule_i + 1;
end;
  trapezoidal_rule_y := trapezoidal_rule_y + ((trapezoidal_rule_h / 2) * f(trapezoidal_rule_b));
  exit(trapezoidal_rule_y);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  a := 0;
  b := 1;
  steps := 10;
  boundary := [a, b];
  y := trapezoidal_rule(boundary, steps);
  writeln('y = ' + FloatToStr(y));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
