{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
type Matrix = record
  data: array of RealArray;
  rows: int64;
  cols: int64;
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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
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
function makeMatrix(data: RealArrayArray; rows: int64; cols: int64): Matrix; forward;
function make_matrix(make_matrix_rows: int64; make_matrix_cols: int64; make_matrix_value: real): Matrix; forward;
function matrix_from_lists(matrix_from_lists_vals: RealArrayArray): Matrix; forward;
function matrix_to_string(matrix_to_string_m: Matrix): string; forward;
function matrix_add(matrix_add_a: Matrix; matrix_add_b: Matrix): Matrix; forward;
function matrix_sub(matrix_sub_a: Matrix; matrix_sub_b: Matrix): Matrix; forward;
function matrix_mul_scalar(matrix_mul_scalar_m: Matrix; matrix_mul_scalar_k: real): Matrix; forward;
function matrix_mul(matrix_mul_a: Matrix; matrix_mul_b: Matrix): Matrix; forward;
function matrix_transpose(matrix_transpose_m: Matrix): Matrix; forward;
function sherman_morrison(sherman_morrison_ainv: Matrix; sherman_morrison_u: Matrix; sherman_morrison_v: Matrix): Matrix; forward;
procedure main(); forward;
function makeMatrix(data: RealArrayArray; rows: int64; cols: int64): Matrix;
begin
  Result.data := data;
  Result.rows := rows;
  Result.cols := cols;
end;
function make_matrix(make_matrix_rows: int64; make_matrix_cols: int64; make_matrix_value: real): Matrix;
var
  make_matrix_arr: array of RealArray;
  make_matrix_r: int64;
  make_matrix_row: array of real;
  make_matrix_c: int64;
begin
  make_matrix_arr := [];
  make_matrix_r := 0;
  while make_matrix_r < make_matrix_rows do begin
  make_matrix_row := [];
  make_matrix_c := 0;
  while make_matrix_c < make_matrix_cols do begin
  make_matrix_row := concat(make_matrix_row, [make_matrix_value]);
  make_matrix_c := make_matrix_c + 1;
end;
  make_matrix_arr := concat(make_matrix_arr, [make_matrix_row]);
  make_matrix_r := make_matrix_r + 1;
end;
  exit(makeMatrix(make_matrix_arr, make_matrix_rows, make_matrix_cols));
end;
function matrix_from_lists(matrix_from_lists_vals: RealArrayArray): Matrix;
var
  matrix_from_lists_r: integer;
  matrix_from_lists_c: int64;
begin
  matrix_from_lists_r := Length(matrix_from_lists_vals);
  if matrix_from_lists_r = 0 then begin
  matrix_from_lists_c := 0;
end else begin
  matrix_from_lists_c := Length(matrix_from_lists_vals[0]);
end;
  exit(makeMatrix(matrix_from_lists_vals, matrix_from_lists_r, matrix_from_lists_c));
end;
function matrix_to_string(matrix_to_string_m: Matrix): string;
var
  matrix_to_string_s: string;
  matrix_to_string_i: int64;
  matrix_to_string_j: int64;
begin
  matrix_to_string_s := '';
  matrix_to_string_i := 0;
  while matrix_to_string_i < matrix_to_string_m.rows do begin
  matrix_to_string_s := matrix_to_string_s + '[';
  matrix_to_string_j := 0;
  while matrix_to_string_j < matrix_to_string_m.cols do begin
  matrix_to_string_s := matrix_to_string_s + FloatToStr(matrix_to_string_m.data[matrix_to_string_i][matrix_to_string_j]);
  if matrix_to_string_j < (matrix_to_string_m.cols - 1) then begin
  matrix_to_string_s := matrix_to_string_s + ', ';
end;
  matrix_to_string_j := matrix_to_string_j + 1;
end;
  matrix_to_string_s := matrix_to_string_s + ']';
  if matrix_to_string_i < (matrix_to_string_m.rows - 1) then begin
  matrix_to_string_s := matrix_to_string_s + #10;
end;
  matrix_to_string_i := matrix_to_string_i + 1;
end;
  exit(matrix_to_string_s);
end;
function matrix_add(matrix_add_a: Matrix; matrix_add_b: Matrix): Matrix;
var
  matrix_add_res: array of RealArray;
  matrix_add_i: int64;
  matrix_add_row: array of real;
  matrix_add_j: int64;
begin
  if (matrix_add_a.rows <> matrix_add_b.rows) or (matrix_add_a.cols <> matrix_add_b.cols) then begin
  exit(makeMatrix([], 0, 0));
end;
  matrix_add_res := [];
  matrix_add_i := 0;
  while matrix_add_i < matrix_add_a.rows do begin
  matrix_add_row := [];
  matrix_add_j := 0;
  while matrix_add_j < matrix_add_a.cols do begin
  matrix_add_row := concat(matrix_add_row, [matrix_add_a.data[matrix_add_i][matrix_add_j] + matrix_add_b.data[matrix_add_i][matrix_add_j]]);
  matrix_add_j := matrix_add_j + 1;
end;
  matrix_add_res := concat(matrix_add_res, [matrix_add_row]);
  matrix_add_i := matrix_add_i + 1;
end;
  exit(makeMatrix(matrix_add_res, matrix_add_a.rows, matrix_add_a.cols));
end;
function matrix_sub(matrix_sub_a: Matrix; matrix_sub_b: Matrix): Matrix;
var
  matrix_sub_res: array of RealArray;
  matrix_sub_i: int64;
  matrix_sub_row: array of real;
  matrix_sub_j: int64;
begin
  if (matrix_sub_a.rows <> matrix_sub_b.rows) or (matrix_sub_a.cols <> matrix_sub_b.cols) then begin
  exit(makeMatrix([], 0, 0));
end;
  matrix_sub_res := [];
  matrix_sub_i := 0;
  while matrix_sub_i < matrix_sub_a.rows do begin
  matrix_sub_row := [];
  matrix_sub_j := 0;
  while matrix_sub_j < matrix_sub_a.cols do begin
  matrix_sub_row := concat(matrix_sub_row, [matrix_sub_a.data[matrix_sub_i][matrix_sub_j] - matrix_sub_b.data[matrix_sub_i][matrix_sub_j]]);
  matrix_sub_j := matrix_sub_j + 1;
end;
  matrix_sub_res := concat(matrix_sub_res, [matrix_sub_row]);
  matrix_sub_i := matrix_sub_i + 1;
end;
  exit(makeMatrix(matrix_sub_res, matrix_sub_a.rows, matrix_sub_a.cols));
end;
function matrix_mul_scalar(matrix_mul_scalar_m: Matrix; matrix_mul_scalar_k: real): Matrix;
var
  matrix_mul_scalar_res: array of RealArray;
  matrix_mul_scalar_i: int64;
  matrix_mul_scalar_row: array of real;
  matrix_mul_scalar_j: int64;
begin
  matrix_mul_scalar_res := [];
  matrix_mul_scalar_i := 0;
  while matrix_mul_scalar_i < matrix_mul_scalar_m.rows do begin
  matrix_mul_scalar_row := [];
  matrix_mul_scalar_j := 0;
  while matrix_mul_scalar_j < matrix_mul_scalar_m.cols do begin
  matrix_mul_scalar_row := concat(matrix_mul_scalar_row, [matrix_mul_scalar_m.data[matrix_mul_scalar_i][matrix_mul_scalar_j] * matrix_mul_scalar_k]);
  matrix_mul_scalar_j := matrix_mul_scalar_j + 1;
end;
  matrix_mul_scalar_res := concat(matrix_mul_scalar_res, [matrix_mul_scalar_row]);
  matrix_mul_scalar_i := matrix_mul_scalar_i + 1;
end;
  exit(makeMatrix(matrix_mul_scalar_res, matrix_mul_scalar_m.rows, matrix_mul_scalar_m.cols));
end;
function matrix_mul(matrix_mul_a: Matrix; matrix_mul_b: Matrix): Matrix;
var
  matrix_mul_res: array of RealArray;
  matrix_mul_i: int64;
  matrix_mul_row: array of real;
  matrix_mul_j: int64;
  matrix_mul_sum: real;
  matrix_mul_k: int64;
begin
  if matrix_mul_a.cols <> matrix_mul_b.rows then begin
  exit(makeMatrix([], 0, 0));
end;
  matrix_mul_res := [];
  matrix_mul_i := 0;
  while matrix_mul_i < matrix_mul_a.rows do begin
  matrix_mul_row := [];
  matrix_mul_j := 0;
  while matrix_mul_j < matrix_mul_b.cols do begin
  matrix_mul_sum := 0;
  matrix_mul_k := 0;
  while matrix_mul_k < matrix_mul_a.cols do begin
  matrix_mul_sum := matrix_mul_sum + (matrix_mul_a.data[matrix_mul_i][matrix_mul_k] * matrix_mul_b.data[matrix_mul_k][matrix_mul_j]);
  matrix_mul_k := matrix_mul_k + 1;
end;
  matrix_mul_row := concat(matrix_mul_row, [matrix_mul_sum]);
  matrix_mul_j := matrix_mul_j + 1;
end;
  matrix_mul_res := concat(matrix_mul_res, [matrix_mul_row]);
  matrix_mul_i := matrix_mul_i + 1;
end;
  exit(makeMatrix(matrix_mul_res, matrix_mul_a.rows, matrix_mul_b.cols));
end;
function matrix_transpose(matrix_transpose_m: Matrix): Matrix;
var
  matrix_transpose_res: array of RealArray;
  matrix_transpose_c: int64;
  matrix_transpose_row: array of real;
  matrix_transpose_r: int64;
begin
  matrix_transpose_res := [];
  matrix_transpose_c := 0;
  while matrix_transpose_c < matrix_transpose_m.cols do begin
  matrix_transpose_row := [];
  matrix_transpose_r := 0;
  while matrix_transpose_r < matrix_transpose_m.rows do begin
  matrix_transpose_row := concat(matrix_transpose_row, [matrix_transpose_m.data[matrix_transpose_r][matrix_transpose_c]]);
  matrix_transpose_r := matrix_transpose_r + 1;
end;
  matrix_transpose_res := concat(matrix_transpose_res, [matrix_transpose_row]);
  matrix_transpose_c := matrix_transpose_c + 1;
end;
  exit(makeMatrix(matrix_transpose_res, matrix_transpose_m.cols, matrix_transpose_m.rows));
end;
function sherman_morrison(sherman_morrison_ainv: Matrix; sherman_morrison_u: Matrix; sherman_morrison_v: Matrix): Matrix;
var
  sherman_morrison_vt: Matrix;
  sherman_morrison_vu: Matrix;
  sherman_morrison_factor: real;
  sherman_morrison_term1: Matrix;
  sherman_morrison_term2: Matrix;
  sherman_morrison_numerator: Matrix;
  sherman_morrison_scaled: Matrix;
begin
  sherman_morrison_vt := matrix_transpose(sherman_morrison_v);
  sherman_morrison_vu := matrix_mul(matrix_mul(sherman_morrison_vt, sherman_morrison_ainv), sherman_morrison_u);
  sherman_morrison_factor := sherman_morrison_vu.data[0][0] + 1;
  if sherman_morrison_factor = 0 then begin
  exit(makeMatrix([], 0, 0));
end;
  sherman_morrison_term1 := matrix_mul(sherman_morrison_ainv, sherman_morrison_u);
  sherman_morrison_term2 := matrix_mul(sherman_morrison_vt, sherman_morrison_ainv);
  sherman_morrison_numerator := matrix_mul(sherman_morrison_term1, sherman_morrison_term2);
  sherman_morrison_scaled := matrix_mul_scalar(sherman_morrison_numerator, 1 / sherman_morrison_factor);
  exit(matrix_sub(sherman_morrison_ainv, sherman_morrison_scaled));
end;
procedure main();
var
  main_ainv: Matrix;
  main_u: Matrix;
  main_v: Matrix;
  main_result_: Matrix;
begin
  main_ainv := matrix_from_lists([[1, 0, 0], [0, 1, 0], [0, 0, 1]]);
  main_u := matrix_from_lists([[1], [2], [-3]]);
  main_v := matrix_from_lists([[4], [-2], [5]]);
  main_result_ := sherman_morrison(main_ainv, main_u, main_v);
  writeln(matrix_to_string(main_result_));
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
