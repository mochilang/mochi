{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
type RealArrayArray = array of RealArray;
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
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  c: real;
  v: RealArray;
  x: real;
  velocity: real;
  vec: RealArray;
  mat: RealArrayArray;
  event: RealArray;
function sqrtApprox(x: real): real; forward;
function beta(velocity: real): real; forward;
function gamma(velocity: real): real; forward;
function transformation_matrix(velocity: real): RealArrayArray; forward;
function mat_vec_mul(mat: RealArrayArray; vec: RealArray): RealArray; forward;
function transform(velocity: real; event: RealArray): RealArray; forward;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrtApprox_guess := x / 2;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function beta(velocity: real): real;
begin
  if velocity > c then begin
  panic('Speed must not exceed light speed 299,792,458 [m/s]!');
end;
  if velocity < 1 then begin
  panic('Speed must be greater than or equal to 1!');
end;
  exit(velocity / c);
end;
function gamma(velocity: real): real;
var
  gamma_b: real;
begin
  gamma_b := beta(velocity);
  exit(1 / sqrtApprox(1 - (gamma_b * gamma_b)));
end;
function transformation_matrix(velocity: real): RealArrayArray;
var
  transformation_matrix_g: real;
  transformation_matrix_b: real;
begin
  transformation_matrix_g := gamma(velocity);
  transformation_matrix_b := beta(velocity);
  exit([[transformation_matrix_g, -transformation_matrix_g * transformation_matrix_b, 0, 0], [-transformation_matrix_g * transformation_matrix_b, transformation_matrix_g, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]);
end;
function mat_vec_mul(mat: RealArrayArray; vec: RealArray): RealArray;
var
  mat_vec_mul_res: array of real;
  mat_vec_mul_i: integer;
  mat_vec_mul_row: array of real;
  mat_vec_mul_value: real;
begin
  mat_vec_mul_res := [];
  mat_vec_mul_i := 0;
  while mat_vec_mul_i < 4 do begin
  mat_vec_mul_row := mat[mat_vec_mul_i];
  mat_vec_mul_value := (((mat_vec_mul_row[0] * vec[0]) + (mat_vec_mul_row[1] * vec[1])) + (mat_vec_mul_row[2] * vec[2])) + (mat_vec_mul_row[3] * vec[3]);
  mat_vec_mul_res := concat(mat_vec_mul_res, [mat_vec_mul_value]);
  mat_vec_mul_i := mat_vec_mul_i + 1;
end;
  exit(mat_vec_mul_res);
end;
function transform(velocity: real; event: RealArray): RealArray;
var
  transform_g: real;
  transform_b: real;
  transform_ct: real;
  transform_x: real;
begin
  transform_g := gamma(velocity);
  transform_b := beta(velocity);
  transform_ct := event[0] * c;
  transform_x := event[1];
  exit([(transform_g * transform_ct) - ((transform_g * transform_b) * transform_x), ((-transform_g * transform_b) * transform_ct) + (transform_g * transform_x), event[2], event[3]]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  c := 2.99792458e+08;
  writeln(FloatToStr(beta(c)));
  writeln(FloatToStr(beta(1.99792458e+08)));
  writeln(FloatToStr(beta(100000)));
  writeln(FloatToStr(gamma(4)));
  writeln(FloatToStr(gamma(100000)));
  writeln(FloatToStr(gamma(3e+07)));
  writeln(list_int_to_str(transformation_matrix(2.9979245e+07)));
  v := transform(2.9979245e+07, [1, 2, 3, 4]);
  writeln(list_real_to_str(v));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
