{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Point = record
  x: real;
  y: real;
  z: real;
end;
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
  a: Point;
  b: Point;
  p: Point;
  x: real;
function makePoint(x: real; y: real; z: real): Point; forward;
function absf(x: real): real; forward;
function sqrt_approx(x: real): real; forward;
function distance(a: Point; b: Point): real; forward;
function point_to_string(p: Point): string; forward;
procedure test_distance(); forward;
procedure main(); forward;
function makePoint(x: real; y: real; z: real): Point;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;
function absf(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function sqrt_approx(x: real): real;
var
  sqrt_approx_guess: real;
  sqrt_approx_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrt_approx_guess := x / 2;
  sqrt_approx_i := 0;
  while sqrt_approx_i < 20 do begin
  sqrt_approx_guess := (sqrt_approx_guess + (x / sqrt_approx_guess)) / 2;
  sqrt_approx_i := sqrt_approx_i + 1;
end;
  exit(sqrt_approx_guess);
end;
function distance(a: Point; b: Point): real;
var
  distance_dx: real;
  distance_dy: real;
  distance_dz: real;
begin
  distance_dx := b.x - a.x;
  distance_dy := b.y - a.y;
  distance_dz := b.z - a.z;
  exit(sqrt_approx(absf(((distance_dx * distance_dx) + (distance_dy * distance_dy)) + (distance_dz * distance_dz))));
end;
function point_to_string(p: Point): string;
begin
  exit(((((('Point(' + FloatToStr(p.x)) + ', ') + FloatToStr(p.y)) + ', ') + FloatToStr(p.z)) + ')');
end;
procedure test_distance();
var
  test_distance_p1: Point;
  test_distance_p2: Point;
  test_distance_d: real;
begin
  test_distance_p1 := makePoint(2, -1, 7);
  test_distance_p2 := makePoint(1, -3, 5);
  test_distance_d := distance(test_distance_p1, test_distance_p2);
  if absf(test_distance_d - 3) > 0.0001 then begin
  panic('distance test failed');
end;
  writeln((((('Distance from ' + point_to_string(test_distance_p1)) + ' to ') + point_to_string(test_distance_p2)) + ' is ') + FloatToStr(test_distance_d));
end;
procedure main();
begin
  test_distance();
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
