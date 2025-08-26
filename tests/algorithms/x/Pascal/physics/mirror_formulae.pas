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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: int64): real;
begin
  _to_float := x;
end;
function to_float(x: int64): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function abs_float(abs_float_x: real): real; forward;
function isclose(isclose_a: real; isclose_b: real; isclose_tolerance: real): boolean; forward;
function focal_length(focal_length_distance_of_object: real; focal_length_distance_of_image: real): real; forward;
function object_distance(object_distance_focal_length_var: real; object_distance_distance_of_image: real): real; forward;
function image_distance(image_distance_focal_length_var: real; image_distance_distance_of_object: real): real; forward;
procedure test_focal_length(); forward;
procedure test_object_distance(); forward;
procedure test_image_distance(); forward;
procedure main(); forward;
function abs_float(abs_float_x: real): real;
begin
  if abs_float_x < 0 then begin
  exit(-abs_float_x);
end;
  exit(abs_float_x);
end;
function isclose(isclose_a: real; isclose_b: real; isclose_tolerance: real): boolean;
begin
  exit(abs_float(isclose_a - isclose_b) < isclose_tolerance);
end;
function focal_length(focal_length_distance_of_object: real; focal_length_distance_of_image: real): real;
begin
  if (focal_length_distance_of_object = 0) or (focal_length_distance_of_image = 0) then begin
  panic('Invalid inputs. Enter non zero values with respect to the sign convention.');
end;
  exit(1 / ((1 / focal_length_distance_of_object) + (1 / focal_length_distance_of_image)));
end;
function object_distance(object_distance_focal_length_var: real; object_distance_distance_of_image: real): real;
begin
  if (object_distance_distance_of_image = 0) or (object_distance_focal_length_var = 0) then begin
  panic('Invalid inputs. Enter non zero values with respect to the sign convention.');
end;
  exit(1 / ((1 / object_distance_focal_length_var) - (1 / object_distance_distance_of_image)));
end;
function image_distance(image_distance_focal_length_var: real; image_distance_distance_of_object: real): real;
begin
  if (image_distance_distance_of_object = 0) or (image_distance_focal_length_var = 0) then begin
  panic('Invalid inputs. Enter non zero values with respect to the sign convention.');
end;
  exit(1 / ((1 / image_distance_focal_length_var) - (1 / image_distance_distance_of_object)));
end;
procedure test_focal_length();
var
  test_focal_length_f1: real;
  test_focal_length_f2: real;
begin
  test_focal_length_f1 := focal_length(10, 20);
  if not isclose(test_focal_length_f1, 6.66666666666666, 1e-08) then begin
  panic('focal_length test1 failed');
end;
  test_focal_length_f2 := focal_length(9.5, 6.7);
  if not isclose(test_focal_length_f2, 3.929012346, 1e-08) then begin
  panic('focal_length test2 failed');
end;
end;
procedure test_object_distance();
var
  test_object_distance_u1: real;
  test_object_distance_u2: real;
begin
  test_object_distance_u1 := object_distance(30, 20);
  if not isclose(test_object_distance_u1, -60, 1e-08) then begin
  panic('object_distance test1 failed');
end;
  test_object_distance_u2 := object_distance(10.5, 11.7);
  if not isclose(test_object_distance_u2, 102.375, 1e-08) then begin
  panic('object_distance test2 failed');
end;
end;
procedure test_image_distance();
var
  test_image_distance_v1: real;
  test_image_distance_v2: real;
begin
  test_image_distance_v1 := image_distance(10, 40);
  if not isclose(test_image_distance_v1, 13.33333333, 1e-08) then begin
  panic('image_distance test1 failed');
end;
  test_image_distance_v2 := image_distance(1.5, 6.7);
  if not isclose(test_image_distance_v2, 1.932692308, 1e-08) then begin
  panic('image_distance test2 failed');
end;
end;
procedure main();
begin
  test_focal_length();
  test_object_distance();
  test_image_distance();
  writeln(FloatToStr(focal_length(10, 20)));
  writeln(FloatToStr(object_distance(30, 20)));
  writeln(FloatToStr(image_distance(10, 40)));
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
  writeln('');
end.
