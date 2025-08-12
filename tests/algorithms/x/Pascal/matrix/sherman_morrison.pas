{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
type Matrix = record
  data: array of RealArray;
  rows: integer;
  cols: integer;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  a: Matrix;
  b: Matrix;
  rows: integer;
  ainv: Matrix;
  cols: integer;
  vals: RealArrayArray;
  v: Matrix;
  value: real;
  m: Matrix;
  k: real;
  u: Matrix;
function makeMatrix(data: RealArrayArray; rows: integer; cols: integer): Matrix; forward;
function make_matrix(rows: integer; cols: integer; value: real): Matrix; forward;
function matrix_from_lists(vals: RealArrayArray): Matrix; forward;
function matrix_to_string(m: Matrix): string; forward;
function matrix_add(a: Matrix; b: Matrix): Matrix; forward;
function matrix_sub(a: Matrix; b: Matrix): Matrix; forward;
function matrix_mul_scalar(m: Matrix; k: real): Matrix; forward;
function matrix_mul(a: Matrix; b: Matrix): Matrix; forward;
function matrix_transpose(m: Matrix): Matrix; forward;
function sherman_morrison(ainv: Matrix; u: Matrix; v: Matrix): Matrix; forward;
procedure main(); forward;
function makeMatrix(data: RealArrayArray; rows: integer; cols: integer): Matrix;
begin
  Result.data := data;
  Result.rows := rows;
  Result.cols := cols;
end;
function make_matrix(rows: integer; cols: integer; value: real): Matrix;
var
  make_matrix_arr: array of RealArray;
  make_matrix_r: integer;
  make_matrix_row: array of real;
  make_matrix_c: integer;
begin
  make_matrix_arr := [];
  make_matrix_r := 0;
  while make_matrix_r < rows do begin
  make_matrix_row := [];
  make_matrix_c := 0;
  while make_matrix_c < cols do begin
  make_matrix_row := concat(make_matrix_row, [value]);
  make_matrix_c := make_matrix_c + 1;
end;
  make_matrix_arr := concat(make_matrix_arr, [make_matrix_row]);
  make_matrix_r := make_matrix_r + 1;
end;
  exit(makeMatrix(make_matrix_arr, rows, cols));
end;
function matrix_from_lists(vals: RealArrayArray): Matrix;
var
  matrix_from_lists_r: integer;
  matrix_from_lists_c: integer;
begin
  matrix_from_lists_r := Length(vals);
  if matrix_from_lists_r = 0 then begin
  matrix_from_lists_c := 0;
end else begin
  matrix_from_lists_c := Length(vals[0]);
end;
  exit(makeMatrix(vals, matrix_from_lists_r, matrix_from_lists_c));
end;
function matrix_to_string(m: Matrix): string;
var
  matrix_to_string_s: string;
  matrix_to_string_i: integer;
  matrix_to_string_j: integer;
begin
  matrix_to_string_s := '';
  matrix_to_string_i := 0;
  while matrix_to_string_i < m.rows do begin
  matrix_to_string_s := matrix_to_string_s + '[';
  matrix_to_string_j := 0;
  while matrix_to_string_j < m.cols do begin
  matrix_to_string_s := matrix_to_string_s + FloatToStr(m.data[matrix_to_string_i][matrix_to_string_j]);
  if matrix_to_string_j < (m.cols - 1) then begin
  matrix_to_string_s := matrix_to_string_s + ', ';
end;
  matrix_to_string_j := matrix_to_string_j + 1;
end;
  matrix_to_string_s := matrix_to_string_s + ']';
  if matrix_to_string_i < (m.rows - 1) then begin
  matrix_to_string_s := matrix_to_string_s + '' + #10 + '';
end;
  matrix_to_string_i := matrix_to_string_i + 1;
end;
  exit(matrix_to_string_s);
end;
function matrix_add(a: Matrix; b: Matrix): Matrix;
var
  matrix_add_res: array of RealArray;
  matrix_add_i: integer;
  matrix_add_row: array of real;
  matrix_add_j: integer;
begin
  if (a.rows <> b.rows) or (a.cols <> b.cols) then begin
  exit(makeMatrix([], 0, 0));
end;
  matrix_add_res := [];
  matrix_add_i := 0;
  while matrix_add_i < a.rows do begin
  matrix_add_row := [];
  matrix_add_j := 0;
  while matrix_add_j < a.cols do begin
  matrix_add_row := concat(matrix_add_row, [a.data[matrix_add_i][matrix_add_j] + b.data[matrix_add_i][matrix_add_j]]);
  matrix_add_j := matrix_add_j + 1;
end;
  matrix_add_res := concat(matrix_add_res, [matrix_add_row]);
  matrix_add_i := matrix_add_i + 1;
end;
  exit(makeMatrix(matrix_add_res, a.rows, a.cols));
end;
function matrix_sub(a: Matrix; b: Matrix): Matrix;
var
  matrix_sub_res: array of RealArray;
  matrix_sub_i: integer;
  matrix_sub_row: array of real;
  matrix_sub_j: integer;
begin
  if (a.rows <> b.rows) or (a.cols <> b.cols) then begin
  exit(makeMatrix([], 0, 0));
end;
  matrix_sub_res := [];
  matrix_sub_i := 0;
  while matrix_sub_i < a.rows do begin
  matrix_sub_row := [];
  matrix_sub_j := 0;
  while matrix_sub_j < a.cols do begin
  matrix_sub_row := concat(matrix_sub_row, [a.data[matrix_sub_i][matrix_sub_j] - b.data[matrix_sub_i][matrix_sub_j]]);
  matrix_sub_j := matrix_sub_j + 1;
end;
  matrix_sub_res := concat(matrix_sub_res, [matrix_sub_row]);
  matrix_sub_i := matrix_sub_i + 1;
end;
  exit(makeMatrix(matrix_sub_res, a.rows, a.cols));
end;
function matrix_mul_scalar(m: Matrix; k: real): Matrix;
var
  matrix_mul_scalar_res: array of RealArray;
  matrix_mul_scalar_i: integer;
  matrix_mul_scalar_row: array of real;
  matrix_mul_scalar_j: integer;
begin
  matrix_mul_scalar_res := [];
  matrix_mul_scalar_i := 0;
  while matrix_mul_scalar_i < m.rows do begin
  matrix_mul_scalar_row := [];
  matrix_mul_scalar_j := 0;
  while matrix_mul_scalar_j < m.cols do begin
  matrix_mul_scalar_row := concat(matrix_mul_scalar_row, [m.data[matrix_mul_scalar_i][matrix_mul_scalar_j] * k]);
  matrix_mul_scalar_j := matrix_mul_scalar_j + 1;
end;
  matrix_mul_scalar_res := concat(matrix_mul_scalar_res, [matrix_mul_scalar_row]);
  matrix_mul_scalar_i := matrix_mul_scalar_i + 1;
end;
  exit(makeMatrix(matrix_mul_scalar_res, m.rows, m.cols));
end;
function matrix_mul(a: Matrix; b: Matrix): Matrix;
var
  matrix_mul_res: array of RealArray;
  matrix_mul_i: integer;
  matrix_mul_row: array of real;
  matrix_mul_j: integer;
  matrix_mul_sum: real;
  matrix_mul_k: integer;
begin
  if a.cols <> b.rows then begin
  exit(makeMatrix([], 0, 0));
end;
  matrix_mul_res := [];
  matrix_mul_i := 0;
  while matrix_mul_i < a.rows do begin
  matrix_mul_row := [];
  matrix_mul_j := 0;
  while matrix_mul_j < b.cols do begin
  matrix_mul_sum := 0;
  matrix_mul_k := 0;
  while matrix_mul_k < a.cols do begin
  matrix_mul_sum := matrix_mul_sum + (a.data[matrix_mul_i][matrix_mul_k] * b.data[matrix_mul_k][matrix_mul_j]);
  matrix_mul_k := matrix_mul_k + 1;
end;
  matrix_mul_row := concat(matrix_mul_row, [matrix_mul_sum]);
  matrix_mul_j := matrix_mul_j + 1;
end;
  matrix_mul_res := concat(matrix_mul_res, [matrix_mul_row]);
  matrix_mul_i := matrix_mul_i + 1;
end;
  exit(makeMatrix(matrix_mul_res, a.rows, b.cols));
end;
function matrix_transpose(m: Matrix): Matrix;
var
  matrix_transpose_res: array of RealArray;
  matrix_transpose_c: integer;
  matrix_transpose_row: array of real;
  matrix_transpose_r: integer;
begin
  matrix_transpose_res := [];
  matrix_transpose_c := 0;
  while matrix_transpose_c < m.cols do begin
  matrix_transpose_row := [];
  matrix_transpose_r := 0;
  while matrix_transpose_r < m.rows do begin
  matrix_transpose_row := concat(matrix_transpose_row, [m.data[matrix_transpose_r][matrix_transpose_c]]);
  matrix_transpose_r := matrix_transpose_r + 1;
end;
  matrix_transpose_res := concat(matrix_transpose_res, [matrix_transpose_row]);
  matrix_transpose_c := matrix_transpose_c + 1;
end;
  exit(makeMatrix(matrix_transpose_res, m.cols, m.rows));
end;
function sherman_morrison(ainv: Matrix; u: Matrix; v: Matrix): Matrix;
var
  sherman_morrison_vt: Matrix;
  sherman_morrison_vu: Matrix;
  sherman_morrison_factor: real;
  sherman_morrison_term1: Matrix;
  sherman_morrison_term2: Matrix;
  sherman_morrison_numerator: Matrix;
  sherman_morrison_scaled: Matrix;
begin
  sherman_morrison_vt := matrix_transpose(v);
  sherman_morrison_vu := matrix_mul(matrix_mul(sherman_morrison_vt, ainv), u);
  sherman_morrison_factor := sherman_morrison_vu.data[0][0] + 1;
  if sherman_morrison_factor = 0 then begin
  exit(makeMatrix([], 0, 0));
end;
  sherman_morrison_term1 := matrix_mul(ainv, u);
  sherman_morrison_term2 := matrix_mul(sherman_morrison_vt, ainv);
  sherman_morrison_numerator := matrix_mul(sherman_morrison_term1, sherman_morrison_term2);
  sherman_morrison_scaled := matrix_mul_scalar(sherman_morrison_numerator, 1 / sherman_morrison_factor);
  exit(matrix_sub(ainv, sherman_morrison_scaled));
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
end.
