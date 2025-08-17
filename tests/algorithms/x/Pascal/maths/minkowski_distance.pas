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
  base: real;
  exp_: integer;
  n: integer;
  order: integer;
  point_a: RealArray;
  point_b: RealArray;
  value: real;
  x: real;
function abs_val(x: real): real; forward;
function pow_float(base: real; pow_float_exp_: integer): real; forward;
function nth_root(value: real; n: integer): real; forward;
function minkowski_distance(point_a: RealArray; point_b: RealArray; order: integer): real; forward;
procedure test_minkowski(); forward;
procedure main(); forward;
function abs_val(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function pow_float(base: real; pow_float_exp_: integer): real;
var
  pow_float_result_: real;
  pow_float_i: integer;
begin
  pow_float_result_ := 1;
  pow_float_i := 0;
  while pow_float_i < exp_ do begin
  pow_float_result_ := pow_float_result_ * base;
  pow_float_i := pow_float_i + 1;
end;
  exit(pow_float_result_);
end;
function nth_root(value: real; n: integer): real;
var
  nth_root_x: real;
  nth_root_i: integer;
  nth_root_num: real;
begin
  if value = 0 then begin
  exit(0);
end;
  nth_root_x := value / Double(n);
  nth_root_i := 0;
  while nth_root_i < 20 do begin
  nth_root_num := (Double(n - 1) * nth_root_x) + (value / pow_float(nth_root_x, n - 1));
  nth_root_x := nth_root_num / Double(n);
  nth_root_i := nth_root_i + 1;
end;
  exit(nth_root_x);
end;
function minkowski_distance(point_a: RealArray; point_b: RealArray; order: integer): real;
var
  minkowski_distance_total: real;
  minkowski_distance_idx: integer;
  minkowski_distance_diff: real;
begin
  if order < 1 then begin
  panic('The order must be greater than or equal to 1.');
end;
  if Length(point_a) <> Length(point_b) then begin
  panic('Both points must have the same dimension.');
end;
  minkowski_distance_total := 0;
  minkowski_distance_idx := 0;
  while minkowski_distance_idx < Length(point_a) do begin
  minkowski_distance_diff := abs_val(point_a[minkowski_distance_idx] - point_b[minkowski_distance_idx]);
  minkowski_distance_total := minkowski_distance_total + pow_float(minkowski_distance_diff, order);
  minkowski_distance_idx := minkowski_distance_idx + 1;
end;
  exit(nth_root(minkowski_distance_total, order));
end;
procedure test_minkowski();
begin
  if abs_val(minkowski_distance([1, 1], [2, 2], 1) - 2) > 0.0001 then begin
  panic('minkowski_distance test1 failed');
end;
  if abs_val(minkowski_distance([1, 2, 3, 4], [5, 6, 7, 8], 2) - 8) > 0.0001 then begin
  panic('minkowski_distance test2 failed');
end;
end;
procedure main();
begin
  test_minkowski();
  writeln(minkowski_distance([5], [0], 3));
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
