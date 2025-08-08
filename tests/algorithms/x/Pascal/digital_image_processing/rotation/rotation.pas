{$mode objfpc}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  img: array of IntArray;
  pts1: array of RealArray;
  pts2: array of RealArray;
  rotated: IntArrayArray;
  rows: integer;
  value: integer;
  cols: integer;
  pt2: RealArrayArray;
  pt1: RealArrayArray;
  x: real;
  m: RealArrayArray;
  v: RealArray;
function mat_inverse3(m: RealArrayArray): RealArrayArray; forward;
function mat_vec_mul(m: RealArrayArray; v: RealArray): RealArray; forward;
function create_matrix(rows: integer; cols: integer; value: integer): IntArrayArray; forward;
function round_to_int(x: real): integer; forward;
function get_rotation(img: IntArrayArray; pt1: RealArrayArray; pt2: RealArrayArray; rows: integer; cols: integer): IntArrayArray; forward;
function mat_inverse3(m: RealArrayArray): RealArrayArray;
var
  mat_inverse3_a: real;
  mat_inverse3_b: real;
  mat_inverse3_c: real;
  mat_inverse3_d: real;
  mat_inverse3_e: real;
  mat_inverse3_f: real;
  mat_inverse3_g: real;
  mat_inverse3_h: real;
  mat_inverse3_i: real;
  mat_inverse3_det: real;
  mat_inverse3_adj00: real;
  mat_inverse3_adj01: real;
  mat_inverse3_adj02: real;
  mat_inverse3_adj10: real;
  mat_inverse3_adj11: real;
  mat_inverse3_adj12: real;
  mat_inverse3_adj20: real;
  mat_inverse3_adj21: real;
  mat_inverse3_adj22: real;
  mat_inverse3_inv: array of RealArray;
begin
  mat_inverse3_a := m[0][0];
  mat_inverse3_b := m[0][1];
  mat_inverse3_c := m[0][2];
  mat_inverse3_d := m[1][0];
  mat_inverse3_e := m[1][1];
  mat_inverse3_f := m[1][2];
  mat_inverse3_g := m[2][0];
  mat_inverse3_h := m[2][1];
  mat_inverse3_i := m[2][2];
  mat_inverse3_det := ((mat_inverse3_a * ((mat_inverse3_e * mat_inverse3_i) - (mat_inverse3_f * mat_inverse3_h))) - (mat_inverse3_b * ((mat_inverse3_d * mat_inverse3_i) - (mat_inverse3_f * mat_inverse3_g)))) + (mat_inverse3_c * ((mat_inverse3_d * mat_inverse3_h) - (mat_inverse3_e * mat_inverse3_g)));
  if mat_inverse3_det = 0 then begin
  panic('singular matrix');
end;
  mat_inverse3_adj00 := (mat_inverse3_e * mat_inverse3_i) - (mat_inverse3_f * mat_inverse3_h);
  mat_inverse3_adj01 := (mat_inverse3_c * mat_inverse3_h) - (mat_inverse3_b * mat_inverse3_i);
  mat_inverse3_adj02 := (mat_inverse3_b * mat_inverse3_f) - (mat_inverse3_c * mat_inverse3_e);
  mat_inverse3_adj10 := (mat_inverse3_f * mat_inverse3_g) - (mat_inverse3_d * mat_inverse3_i);
  mat_inverse3_adj11 := (mat_inverse3_a * mat_inverse3_i) - (mat_inverse3_c * mat_inverse3_g);
  mat_inverse3_adj12 := (mat_inverse3_c * mat_inverse3_d) - (mat_inverse3_a * mat_inverse3_f);
  mat_inverse3_adj20 := (mat_inverse3_d * mat_inverse3_h) - (mat_inverse3_e * mat_inverse3_g);
  mat_inverse3_adj21 := (mat_inverse3_b * mat_inverse3_g) - (mat_inverse3_a * mat_inverse3_h);
  mat_inverse3_adj22 := (mat_inverse3_a * mat_inverse3_e) - (mat_inverse3_b * mat_inverse3_d);
  mat_inverse3_inv := [];
  mat_inverse3_inv := concat(mat_inverse3_inv, [[mat_inverse3_adj00 / mat_inverse3_det, mat_inverse3_adj01 / mat_inverse3_det, mat_inverse3_adj02 / mat_inverse3_det]]);
  mat_inverse3_inv := concat(mat_inverse3_inv, [[mat_inverse3_adj10 / mat_inverse3_det, mat_inverse3_adj11 / mat_inverse3_det, mat_inverse3_adj12 / mat_inverse3_det]]);
  mat_inverse3_inv := concat(mat_inverse3_inv, [[mat_inverse3_adj20 / mat_inverse3_det, mat_inverse3_adj21 / mat_inverse3_det, mat_inverse3_adj22 / mat_inverse3_det]]);
  exit(mat_inverse3_inv);
end;
function mat_vec_mul(m: RealArrayArray; v: RealArray): RealArray;
var
  mat_vec_mul_res: array of real;
  mat_vec_mul_i: integer;
  mat_vec_mul_val: real;
begin
  mat_vec_mul_res := [];
  mat_vec_mul_i := 0;
  while mat_vec_mul_i < 3 do begin
  mat_vec_mul_val := ((m[mat_vec_mul_i][0] * v[0]) + (m[mat_vec_mul_i][1] * v[1])) + (m[mat_vec_mul_i][2] * v[2]);
  mat_vec_mul_res := concat(mat_vec_mul_res, [mat_vec_mul_val]);
  mat_vec_mul_i := mat_vec_mul_i + 1;
end;
  exit(mat_vec_mul_res);
end;
function create_matrix(rows: integer; cols: integer; value: integer): IntArrayArray;
var
  create_matrix_result_: array of IntArray;
  create_matrix_r: integer;
  create_matrix_row: array of integer;
  create_matrix_c: integer;
begin
  create_matrix_result_ := [];
  create_matrix_r := 0;
  while create_matrix_r < rows do begin
  create_matrix_row := [];
  create_matrix_c := 0;
  while create_matrix_c < cols do begin
  create_matrix_row := concat(create_matrix_row, IntArray([value]));
  create_matrix_c := create_matrix_c + 1;
end;
  create_matrix_result_ := concat(create_matrix_result_, [create_matrix_row]);
  create_matrix_r := create_matrix_r + 1;
end;
  exit(create_matrix_result_);
end;
function round_to_int(x: real): integer;
begin
  if x >= 0 then begin
  exit(Trunc(x + 0.5));
end;
  exit(Trunc(x - 0.5));
end;
function get_rotation(img: IntArrayArray; pt1: RealArrayArray; pt2: RealArrayArray; rows: integer; cols: integer): IntArrayArray;
var
  get_rotation_src: array of array of real;
  get_rotation_inv: RealArrayArray;
  get_rotation_vecx: array of real;
  get_rotation_vecy: array of real;
  get_rotation_avec: RealArray;
  get_rotation_bvec: RealArray;
  get_rotation_a0: real;
  get_rotation_a1: real;
  get_rotation_a2: real;
  get_rotation_b0: real;
  get_rotation_b1: real;
  get_rotation_b2: real;
  get_rotation_out: IntArrayArray;
  get_rotation_y: integer;
  get_rotation_x: integer;
  get_rotation_xf: real;
  get_rotation_yf: real;
  get_rotation_sx: integer;
  get_rotation_sy: integer;
begin
  get_rotation_src := [[pt1[0][0], pt1[0][1], 1], [pt1[1][0], pt1[1][1], 1], [pt1[2][0], pt1[2][1], 1]];
  get_rotation_inv := mat_inverse3(get_rotation_src);
  get_rotation_vecx := [pt2[0][0], pt2[1][0], pt2[2][0]];
  get_rotation_vecy := [pt2[0][1], pt2[1][1], pt2[2][1]];
  get_rotation_avec := mat_vec_mul(get_rotation_inv, get_rotation_vecx);
  get_rotation_bvec := mat_vec_mul(get_rotation_inv, get_rotation_vecy);
  get_rotation_a0 := get_rotation_avec[0];
  get_rotation_a1 := get_rotation_avec[1];
  get_rotation_a2 := get_rotation_avec[2];
  get_rotation_b0 := get_rotation_bvec[0];
  get_rotation_b1 := get_rotation_bvec[1];
  get_rotation_b2 := get_rotation_bvec[2];
  get_rotation_out := create_matrix(rows, cols, 0);
  get_rotation_y := 0;
  while get_rotation_y < rows do begin
  get_rotation_x := 0;
  while get_rotation_x < cols do begin
  get_rotation_xf := ((get_rotation_a0 * (1 * get_rotation_x)) + (get_rotation_a1 * (1 * get_rotation_y))) + get_rotation_a2;
  get_rotation_yf := ((get_rotation_b0 * (1 * get_rotation_x)) + (get_rotation_b1 * (1 * get_rotation_y))) + get_rotation_b2;
  get_rotation_sx := round_to_int(get_rotation_xf);
  get_rotation_sy := round_to_int(get_rotation_yf);
  if (((get_rotation_sx >= 0) and (get_rotation_sx < cols)) and (get_rotation_sy >= 0)) and (get_rotation_sy < rows) then begin
  get_rotation_out[get_rotation_sy][get_rotation_sx] := img[get_rotation_y][get_rotation_x];
end;
  get_rotation_x := get_rotation_x + 1;
end;
  get_rotation_y := get_rotation_y + 1;
end;
  exit(get_rotation_out);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  img := [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
  pts1 := [[0, 0], [2, 0], [0, 2]];
  pts2 := [[0, 2], [0, 0], [2, 2]];
  rotated := get_rotation(img, pts1, pts2, 3, 3);
  writeln(list_list_int_to_str(rotated));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
