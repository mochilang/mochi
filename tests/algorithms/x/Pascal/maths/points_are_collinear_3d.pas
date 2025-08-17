{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Point3d = record
  x: real;
  y: real;
  z: real;
end;
type Vector3d = record
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
  a: Point3d;
  ab: Vector3d;
  ac: Vector3d;
  accuracy: integer;
  b: Point3d;
  c: Point3d;
  digits: integer;
  exp_: integer;
  p1: Point3d;
  p2: Point3d;
  v: Vector3d;
  x: real;
function makeVector3d(x: real; y: real; z: real): Vector3d; forward;
function makePoint3d(x: real; y: real; z: real): Point3d; forward;
function create_vector(p1: Point3d; p2: Point3d): Vector3d; forward;
function get_3d_vectors_cross(ab: Vector3d; ac: Vector3d): Vector3d; forward;
function pow10(pow10_exp_: integer): real; forward;
function round_float(x: real; digits: integer): real; forward;
function is_zero_vector(v: Vector3d; accuracy: integer): boolean; forward;
function are_collinear(a: Point3d; b: Point3d; c: Point3d; accuracy: integer): boolean; forward;
procedure test_are_collinear(); forward;
procedure main(); forward;
function makeVector3d(x: real; y: real; z: real): Vector3d;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;
function makePoint3d(x: real; y: real; z: real): Point3d;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;
function create_vector(p1: Point3d; p2: Point3d): Vector3d;
var
  create_vector_vx: real;
  create_vector_vy: real;
  create_vector_vz: real;
begin
  create_vector_vx := p2.x - p1.x;
  create_vector_vy := p2.y - p1.y;
  create_vector_vz := p2.z - p1.z;
  exit(makeVector3d(create_vector_vx, create_vector_vy, create_vector_vz));
end;
function get_3d_vectors_cross(ab: Vector3d; ac: Vector3d): Vector3d;
var
  get_3d_vectors_cross_cx: real;
  get_3d_vectors_cross_cy: real;
  get_3d_vectors_cross_cz: real;
begin
  get_3d_vectors_cross_cx := (ab.y * ac.z) - (ab.z * ac.y);
  get_3d_vectors_cross_cy := (ab.z * ac.x) - (ab.x * ac.z);
  get_3d_vectors_cross_cz := (ab.x * ac.y) - (ab.y * ac.x);
  exit(makeVector3d(get_3d_vectors_cross_cx, get_3d_vectors_cross_cy, get_3d_vectors_cross_cz));
end;
function pow10(pow10_exp_: integer): real;
var
  pow10_result_: real;
  pow10_i: integer;
begin
  pow10_result_ := 1;
  pow10_i := 0;
  while pow10_i < exp_ do begin
  pow10_result_ := pow10_result_ * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_result_);
end;
function round_float(x: real; digits: integer): real;
var
  round_float_factor: real;
  round_float_v: real;
  round_float_t: integer;
begin
  round_float_factor := pow10(digits);
  round_float_v := x * round_float_factor;
  if round_float_v >= 0 then begin
  round_float_v := round_float_v + 0.5;
end else begin
  round_float_v := round_float_v - 0.5;
end;
  round_float_t := Trunc(round_float_v);
  exit(Double(round_float_t) / round_float_factor);
end;
function is_zero_vector(v: Vector3d; accuracy: integer): boolean;
begin
  exit(((round_float(v.x, accuracy) = 0) and (round_float(v.y, accuracy) = 0)) and (round_float(v.z, accuracy) = 0));
end;
function are_collinear(a: Point3d; b: Point3d; c: Point3d; accuracy: integer): boolean;
var
  are_collinear_ab: Vector3d;
  are_collinear_ac: Vector3d;
  are_collinear_cross: Vector3d;
begin
  are_collinear_ab := create_vector(a, b);
  are_collinear_ac := create_vector(a, c);
  are_collinear_cross := get_3d_vectors_cross(are_collinear_ab, are_collinear_ac);
  exit(is_zero_vector(are_collinear_cross, accuracy));
end;
procedure test_are_collinear();
var
  test_are_collinear_p1: Point3d;
  test_are_collinear_p2: Point3d;
  test_are_collinear_p3: Point3d;
  test_are_collinear_q3: Point3d;
begin
  test_are_collinear_p1 := makePoint3d(0, 0, 0);
  test_are_collinear_p2 := makePoint3d(1, 1, 1);
  test_are_collinear_p3 := makePoint3d(2, 2, 2);
  if not are_collinear(test_are_collinear_p1, test_are_collinear_p2, test_are_collinear_p3, 10) then begin
  panic('collinear test failed');
end;
  test_are_collinear_q3 := makePoint3d(1, 2, 3);
  if are_collinear(test_are_collinear_p1, test_are_collinear_p2, test_are_collinear_q3, 10) then begin
  panic('non-collinear test failed');
end;
end;
procedure main();
var
  main_a: Point3d;
  main_b: Point3d;
  main_c: Point3d;
  main_d: Point3d;
  main_e: Point3d;
  main_f: Point3d;
begin
  test_are_collinear();
  main_a := makePoint3d(4.802293498137402, 3.536233125455244, 0);
  main_b := makePoint3d(-2.186788107953106, -9.24561398001649, 7.141509524846482);
  main_c := makePoint3d(1.530169574640268, -2.447927606600034, 3.343487096469054);
  writeln(LowerCase(BoolToStr(are_collinear(main_a, main_b, main_c, 10), true)));
  main_d := makePoint3d(2.399001826862445, -2.452009976680793, 4.464656666157666);
  main_e := makePoint3d(-3.682816335934376, 5.753788986533145, 9.490993909044244);
  main_f := makePoint3d(1.962903518985307, 3.741415730125627, 7);
  writeln(LowerCase(BoolToStr(are_collinear(main_d, main_e, main_f, 10), true)));
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
