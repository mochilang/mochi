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
function list_list_real_to_str(xs: array of RealArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_real_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function makeMatrix(data: RealArrayArray; rows: integer; cols: integer): Matrix; forward;
function make_matrix(make_matrix_values: RealArrayArray): Matrix; forward;
function matrix_columns(matrix_columns_m: Matrix): RealArrayArray; forward;
function matrix_identity(matrix_identity_m: Matrix): Matrix; forward;
function matrix_minor(matrix_minor_m: Matrix; matrix_minor_r: integer; matrix_minor_c: integer): real; forward;
function matrix_cofactor(matrix_cofactor_m: Matrix; matrix_cofactor_r: integer; matrix_cofactor_c: integer): real; forward;
function matrix_minors(matrix_minors_m: Matrix): Matrix; forward;
function matrix_cofactors(matrix_cofactors_m: Matrix): Matrix; forward;
function matrix_determinant(matrix_determinant_m: Matrix): real; forward;
function matrix_is_invertible(matrix_is_invertible_m: Matrix): boolean; forward;
function matrix_adjugate(matrix_adjugate_m: Matrix): Matrix; forward;
function matrix_inverse(matrix_inverse_m: Matrix): Matrix; forward;
function matrix_add_row(matrix_add_row_m: Matrix; matrix_add_row_row: RealArray): Matrix; forward;
function matrix_add_column(matrix_add_column_m: Matrix; matrix_add_column_col: RealArray): Matrix; forward;
function matrix_mul_scalar(matrix_mul_scalar_m: Matrix; matrix_mul_scalar_s: real): Matrix; forward;
function matrix_neg(matrix_neg_m: Matrix): Matrix; forward;
function matrix_add(matrix_add_a: Matrix; matrix_add_b: Matrix): Matrix; forward;
function matrix_sub(matrix_sub_a: Matrix; matrix_sub_b: Matrix): Matrix; forward;
function matrix_dot(matrix_dot_row: RealArray; matrix_dot_col: RealArray): real; forward;
function matrix_mul(matrix_mul_a: Matrix; matrix_mul_b: Matrix): Matrix; forward;
function matrix_pow(matrix_pow_m: Matrix; matrix_pow_p: integer): Matrix; forward;
function matrix_to_string(matrix_to_string_m: Matrix): string; forward;
procedure main(); forward;
function makeMatrix(data: RealArrayArray; rows: integer; cols: integer): Matrix;
begin
  Result.data := data;
  Result.rows := rows;
  Result.cols := cols;
end;
function make_matrix(make_matrix_values: RealArrayArray): Matrix;
var
  make_matrix_r: integer;
  make_matrix_c: integer;
  make_matrix_i: integer;
begin
  make_matrix_r := Length(make_matrix_values);
  if make_matrix_r = 0 then begin
  exit(makeMatrix([], 0, 0));
end;
  make_matrix_c := Length(make_matrix_values[0]);
  make_matrix_i := 0;
  while make_matrix_i < make_matrix_r do begin
  if Length(make_matrix_values[make_matrix_i]) <> make_matrix_c then begin
  exit(makeMatrix([], 0, 0));
end;
  make_matrix_i := make_matrix_i + 1;
end;
  exit(makeMatrix(make_matrix_values, make_matrix_r, make_matrix_c));
end;
function matrix_columns(matrix_columns_m: Matrix): RealArrayArray;
var
  matrix_columns_cols: array of RealArray;
  matrix_columns_j: integer;
  matrix_columns_col: array of real;
  matrix_columns_i: integer;
begin
  matrix_columns_cols := [];
  matrix_columns_j := 0;
  while matrix_columns_j < matrix_columns_m.cols do begin
  matrix_columns_col := [];
  matrix_columns_i := 0;
  while matrix_columns_i < matrix_columns_m.rows do begin
  matrix_columns_col := concat(matrix_columns_col, [matrix_columns_m.data[matrix_columns_i][matrix_columns_j]]);
  matrix_columns_i := matrix_columns_i + 1;
end;
  matrix_columns_cols := concat(matrix_columns_cols, [matrix_columns_col]);
  matrix_columns_j := matrix_columns_j + 1;
end;
  exit(matrix_columns_cols);
end;
function matrix_identity(matrix_identity_m: Matrix): Matrix;
var
  matrix_identity_vals: array of RealArray;
  matrix_identity_i: integer;
  matrix_identity_row: array of real;
  matrix_identity_j: integer;
  matrix_identity_v: real;
begin
  matrix_identity_vals := [];
  matrix_identity_i := 0;
  while matrix_identity_i < matrix_identity_m.rows do begin
  matrix_identity_row := [];
  matrix_identity_j := 0;
  while matrix_identity_j < matrix_identity_m.cols do begin
  if matrix_identity_i = matrix_identity_j then begin
  matrix_identity_v := 1;
end else begin
  matrix_identity_v := 0;
end;
  matrix_identity_row := concat(matrix_identity_row, [matrix_identity_v]);
  matrix_identity_j := matrix_identity_j + 1;
end;
  matrix_identity_vals := concat(matrix_identity_vals, [matrix_identity_row]);
  matrix_identity_i := matrix_identity_i + 1;
end;
  exit(makeMatrix(matrix_identity_vals, matrix_identity_m.rows, matrix_identity_m.cols));
end;
function matrix_minor(matrix_minor_m: Matrix; matrix_minor_r: integer; matrix_minor_c: integer): real;
var
  matrix_minor_vals: array of RealArray;
  matrix_minor_i: integer;
  matrix_minor_row: array of real;
  matrix_minor_j: integer;
  matrix_minor_sub: Matrix;
begin
  matrix_minor_vals := [];
  matrix_minor_i := 0;
  while matrix_minor_i < matrix_minor_m.rows do begin
  if matrix_minor_i <> matrix_minor_r then begin
  matrix_minor_row := [];
  matrix_minor_j := 0;
  while matrix_minor_j < matrix_minor_m.cols do begin
  if matrix_minor_j <> matrix_minor_c then begin
  matrix_minor_row := concat(matrix_minor_row, [matrix_minor_m.data[matrix_minor_i][matrix_minor_j]]);
end;
  matrix_minor_j := matrix_minor_j + 1;
end;
  matrix_minor_vals := concat(matrix_minor_vals, [matrix_minor_row]);
end;
  matrix_minor_i := matrix_minor_i + 1;
end;
  matrix_minor_sub := makeMatrix(matrix_minor_vals, matrix_minor_m.rows - 1, matrix_minor_m.cols - 1);
  exit(matrix_determinant(matrix_minor_sub));
end;
function matrix_cofactor(matrix_cofactor_m: Matrix; matrix_cofactor_r: integer; matrix_cofactor_c: integer): real;
var
  matrix_cofactor_minor: real;
begin
  matrix_cofactor_minor := matrix_minor(matrix_cofactor_m, matrix_cofactor_r, matrix_cofactor_c);
  if ((matrix_cofactor_r + matrix_cofactor_c) mod 2) = 0 then begin
  exit(matrix_cofactor_minor);
end;
  exit(-1 * matrix_cofactor_minor);
end;
function matrix_minors(matrix_minors_m: Matrix): Matrix;
var
  matrix_minors_vals: array of RealArray;
  matrix_minors_i: integer;
  matrix_minors_row: array of real;
  matrix_minors_j: integer;
begin
  matrix_minors_vals := [];
  matrix_minors_i := 0;
  while matrix_minors_i < matrix_minors_m.rows do begin
  matrix_minors_row := [];
  matrix_minors_j := 0;
  while matrix_minors_j < matrix_minors_m.cols do begin
  matrix_minors_row := concat(matrix_minors_row, [matrix_minor(matrix_minors_m, matrix_minors_i, matrix_minors_j)]);
  matrix_minors_j := matrix_minors_j + 1;
end;
  matrix_minors_vals := concat(matrix_minors_vals, [matrix_minors_row]);
  matrix_minors_i := matrix_minors_i + 1;
end;
  exit(makeMatrix(matrix_minors_vals, matrix_minors_m.rows, matrix_minors_m.cols));
end;
function matrix_cofactors(matrix_cofactors_m: Matrix): Matrix;
var
  matrix_cofactors_vals: array of RealArray;
  matrix_cofactors_i: integer;
  matrix_cofactors_row: array of real;
  matrix_cofactors_j: integer;
begin
  matrix_cofactors_vals := [];
  matrix_cofactors_i := 0;
  while matrix_cofactors_i < matrix_cofactors_m.rows do begin
  matrix_cofactors_row := [];
  matrix_cofactors_j := 0;
  while matrix_cofactors_j < matrix_cofactors_m.cols do begin
  matrix_cofactors_row := concat(matrix_cofactors_row, [matrix_cofactor(matrix_cofactors_m, matrix_cofactors_i, matrix_cofactors_j)]);
  matrix_cofactors_j := matrix_cofactors_j + 1;
end;
  matrix_cofactors_vals := concat(matrix_cofactors_vals, [matrix_cofactors_row]);
  matrix_cofactors_i := matrix_cofactors_i + 1;
end;
  exit(makeMatrix(matrix_cofactors_vals, matrix_cofactors_m.rows, matrix_cofactors_m.cols));
end;
function matrix_determinant(matrix_determinant_m: Matrix): real;
var
  matrix_determinant_sum: real;
  matrix_determinant_j: integer;
begin
  if matrix_determinant_m.rows <> matrix_determinant_m.cols then begin
  exit(0);
end;
  if matrix_determinant_m.rows = 0 then begin
  exit(0);
end;
  if matrix_determinant_m.rows = 1 then begin
  exit(matrix_determinant_m.data[0][0]);
end;
  if matrix_determinant_m.rows = 2 then begin
  exit((matrix_determinant_m.data[0][0] * matrix_determinant_m.data[1][1]) - (matrix_determinant_m.data[0][1] * matrix_determinant_m.data[1][0]));
end;
  matrix_determinant_sum := 0;
  matrix_determinant_j := 0;
  while matrix_determinant_j < matrix_determinant_m.cols do begin
  matrix_determinant_sum := matrix_determinant_sum + (matrix_determinant_m.data[0][matrix_determinant_j] * matrix_cofactor(matrix_determinant_m, 0, matrix_determinant_j));
  matrix_determinant_j := matrix_determinant_j + 1;
end;
  exit(matrix_determinant_sum);
end;
function matrix_is_invertible(matrix_is_invertible_m: Matrix): boolean;
begin
  exit(matrix_determinant(matrix_is_invertible_m) <> 0);
end;
function matrix_adjugate(matrix_adjugate_m: Matrix): Matrix;
var
  matrix_adjugate_cof: Matrix;
  matrix_adjugate_vals: array of RealArray;
  matrix_adjugate_i: integer;
  matrix_adjugate_row: array of real;
  matrix_adjugate_j: integer;
begin
  matrix_adjugate_cof := matrix_cofactors(matrix_adjugate_m);
  matrix_adjugate_vals := [];
  matrix_adjugate_i := 0;
  while matrix_adjugate_i < matrix_adjugate_m.rows do begin
  matrix_adjugate_row := [];
  matrix_adjugate_j := 0;
  while matrix_adjugate_j < matrix_adjugate_m.cols do begin
  matrix_adjugate_row := concat(matrix_adjugate_row, [matrix_adjugate_cof.data[matrix_adjugate_j][matrix_adjugate_i]]);
  matrix_adjugate_j := matrix_adjugate_j + 1;
end;
  matrix_adjugate_vals := concat(matrix_adjugate_vals, [matrix_adjugate_row]);
  matrix_adjugate_i := matrix_adjugate_i + 1;
end;
  exit(makeMatrix(matrix_adjugate_vals, matrix_adjugate_m.rows, matrix_adjugate_m.cols));
end;
function matrix_inverse(matrix_inverse_m: Matrix): Matrix;
var
  matrix_inverse_det: real;
  matrix_inverse_adj: Matrix;
begin
  matrix_inverse_det := matrix_determinant(matrix_inverse_m);
  if matrix_inverse_det = 0 then begin
  exit(makeMatrix([], 0, 0));
end;
  matrix_inverse_adj := matrix_adjugate(matrix_inverse_m);
  exit(matrix_mul_scalar(matrix_inverse_adj, 1 / matrix_inverse_det));
end;
function matrix_add_row(matrix_add_row_m: Matrix; matrix_add_row_row: RealArray): Matrix;
var
  matrix_add_row_newData: array of RealArray;
begin
  matrix_add_row_newData := matrix_add_row_m.data;
  matrix_add_row_newData := concat(matrix_add_row_newData, [matrix_add_row_row]);
  exit(makeMatrix(matrix_add_row_newData, matrix_add_row_m.rows + 1, matrix_add_row_m.cols));
end;
function matrix_add_column(matrix_add_column_m: Matrix; matrix_add_column_col: RealArray): Matrix;
var
  matrix_add_column_newData: array of RealArray;
  matrix_add_column_i: integer;
begin
  matrix_add_column_newData := [];
  matrix_add_column_i := 0;
  while matrix_add_column_i < matrix_add_column_m.rows do begin
  matrix_add_column_newData := concat(matrix_add_column_newData, [concat(matrix_add_column_m.data[matrix_add_column_i], [matrix_add_column_col[matrix_add_column_i]])]);
  matrix_add_column_i := matrix_add_column_i + 1;
end;
  exit(makeMatrix(matrix_add_column_newData, matrix_add_column_m.rows, matrix_add_column_m.cols + 1));
end;
function matrix_mul_scalar(matrix_mul_scalar_m: Matrix; matrix_mul_scalar_s: real): Matrix;
var
  matrix_mul_scalar_vals: array of RealArray;
  matrix_mul_scalar_i: integer;
  matrix_mul_scalar_row: array of real;
  matrix_mul_scalar_j: integer;
begin
  matrix_mul_scalar_vals := [];
  matrix_mul_scalar_i := 0;
  while matrix_mul_scalar_i < matrix_mul_scalar_m.rows do begin
  matrix_mul_scalar_row := [];
  matrix_mul_scalar_j := 0;
  while matrix_mul_scalar_j < matrix_mul_scalar_m.cols do begin
  matrix_mul_scalar_row := concat(matrix_mul_scalar_row, [matrix_mul_scalar_m.data[matrix_mul_scalar_i][matrix_mul_scalar_j] * matrix_mul_scalar_s]);
  matrix_mul_scalar_j := matrix_mul_scalar_j + 1;
end;
  matrix_mul_scalar_vals := concat(matrix_mul_scalar_vals, [matrix_mul_scalar_row]);
  matrix_mul_scalar_i := matrix_mul_scalar_i + 1;
end;
  exit(makeMatrix(matrix_mul_scalar_vals, matrix_mul_scalar_m.rows, matrix_mul_scalar_m.cols));
end;
function matrix_neg(matrix_neg_m: Matrix): Matrix;
begin
  exit(matrix_mul_scalar(matrix_neg_m, -1));
end;
function matrix_add(matrix_add_a: Matrix; matrix_add_b: Matrix): Matrix;
var
  matrix_add_vals: array of RealArray;
  matrix_add_i: integer;
  matrix_add_row_var: array of real;
  matrix_add_j: integer;
begin
  if (matrix_add_a.rows <> matrix_add_b.rows) or (matrix_add_a.cols <> matrix_add_b.cols) then begin
  exit(makeMatrix([], 0, 0));
end;
  matrix_add_vals := [];
  matrix_add_i := 0;
  while matrix_add_i < matrix_add_a.rows do begin
  matrix_add_row_var := [];
  matrix_add_j := 0;
  while matrix_add_j < matrix_add_a.cols do begin
  matrix_add_row_var := concat(matrix_add_row_var, [matrix_add_a.data[matrix_add_i][matrix_add_j] + matrix_add_b.data[matrix_add_i][matrix_add_j]]);
  matrix_add_j := matrix_add_j + 1;
end;
  matrix_add_vals := concat(matrix_add_vals, [matrix_add_row_var]);
  matrix_add_i := matrix_add_i + 1;
end;
  exit(makeMatrix(matrix_add_vals, matrix_add_a.rows, matrix_add_a.cols));
end;
function matrix_sub(matrix_sub_a: Matrix; matrix_sub_b: Matrix): Matrix;
var
  matrix_sub_vals: array of RealArray;
  matrix_sub_i: integer;
  matrix_sub_row: array of real;
  matrix_sub_j: integer;
begin
  if (matrix_sub_a.rows <> matrix_sub_b.rows) or (matrix_sub_a.cols <> matrix_sub_b.cols) then begin
  exit(makeMatrix([], 0, 0));
end;
  matrix_sub_vals := [];
  matrix_sub_i := 0;
  while matrix_sub_i < matrix_sub_a.rows do begin
  matrix_sub_row := [];
  matrix_sub_j := 0;
  while matrix_sub_j < matrix_sub_a.cols do begin
  matrix_sub_row := concat(matrix_sub_row, [matrix_sub_a.data[matrix_sub_i][matrix_sub_j] - matrix_sub_b.data[matrix_sub_i][matrix_sub_j]]);
  matrix_sub_j := matrix_sub_j + 1;
end;
  matrix_sub_vals := concat(matrix_sub_vals, [matrix_sub_row]);
  matrix_sub_i := matrix_sub_i + 1;
end;
  exit(makeMatrix(matrix_sub_vals, matrix_sub_a.rows, matrix_sub_a.cols));
end;
function matrix_dot(matrix_dot_row: RealArray; matrix_dot_col: RealArray): real;
var
  matrix_dot_sum: real;
  matrix_dot_i: integer;
begin
  matrix_dot_sum := 0;
  matrix_dot_i := 0;
  while matrix_dot_i < Length(matrix_dot_row) do begin
  matrix_dot_sum := matrix_dot_sum + (matrix_dot_row[matrix_dot_i] * matrix_dot_col[matrix_dot_i]);
  matrix_dot_i := matrix_dot_i + 1;
end;
  exit(matrix_dot_sum);
end;
function matrix_mul(matrix_mul_a: Matrix; matrix_mul_b: Matrix): Matrix;
var
  matrix_mul_bcols: RealArrayArray;
  matrix_mul_vals: array of RealArray;
  matrix_mul_i: integer;
  matrix_mul_row: array of real;
  matrix_mul_j: integer;
begin
  if matrix_mul_a.cols <> matrix_mul_b.rows then begin
  exit(makeMatrix([], 0, 0));
end;
  matrix_mul_bcols := matrix_columns(matrix_mul_b);
  matrix_mul_vals := [];
  matrix_mul_i := 0;
  while matrix_mul_i < matrix_mul_a.rows do begin
  matrix_mul_row := [];
  matrix_mul_j := 0;
  while matrix_mul_j < matrix_mul_b.cols do begin
  matrix_mul_row := concat(matrix_mul_row, [matrix_dot(matrix_mul_a.data[matrix_mul_i], matrix_mul_bcols[matrix_mul_j])]);
  matrix_mul_j := matrix_mul_j + 1;
end;
  matrix_mul_vals := concat(matrix_mul_vals, [matrix_mul_row]);
  matrix_mul_i := matrix_mul_i + 1;
end;
  exit(makeMatrix(matrix_mul_vals, matrix_mul_a.rows, matrix_mul_b.cols));
end;
function matrix_pow(matrix_pow_m: Matrix; matrix_pow_p: integer): Matrix;
var
  matrix_pow_result_: Matrix;
  matrix_pow_i: integer;
begin
  if matrix_pow_p = 0 then begin
  exit(matrix_identity(matrix_pow_m));
end;
  if matrix_pow_p < 0 then begin
  if matrix_is_invertible(matrix_pow_m) then begin
  exit(matrix_pow(matrix_inverse(matrix_pow_m), -matrix_pow_p));
end;
  exit(makeMatrix([], 0, 0));
end;
  matrix_pow_result_ := matrix_pow_m;
  matrix_pow_i := 1;
  while matrix_pow_i < matrix_pow_p do begin
  matrix_pow_result_ := matrix_mul(matrix_pow_result_, matrix_pow_m);
  matrix_pow_i := matrix_pow_i + 1;
end;
  exit(matrix_pow_result_);
end;
function matrix_to_string(matrix_to_string_m: Matrix): string;
var
  matrix_to_string_s: string;
  matrix_to_string_i: integer;
  matrix_to_string_j: integer;
begin
  if matrix_to_string_m.rows = 0 then begin
  exit('[]');
end;
  matrix_to_string_s := '[';
  matrix_to_string_i := 0;
  while matrix_to_string_i < matrix_to_string_m.rows do begin
  matrix_to_string_s := matrix_to_string_s + '[';
  matrix_to_string_j := 0;
  while matrix_to_string_j < matrix_to_string_m.cols do begin
  matrix_to_string_s := matrix_to_string_s + FloatToStr(matrix_to_string_m.data[matrix_to_string_i][matrix_to_string_j]);
  if matrix_to_string_j < (matrix_to_string_m.cols - 1) then begin
  matrix_to_string_s := matrix_to_string_s + ' ';
end;
  matrix_to_string_j := matrix_to_string_j + 1;
end;
  matrix_to_string_s := matrix_to_string_s + ']';
  if matrix_to_string_i < (matrix_to_string_m.rows - 1) then begin
  matrix_to_string_s := matrix_to_string_s + '' + #10 + ' ';
end;
  matrix_to_string_i := matrix_to_string_i + 1;
end;
  matrix_to_string_s := matrix_to_string_s + ']';
  exit(matrix_to_string_s);
end;
procedure main();
var
  main_m: Matrix;
  main_m2: Matrix;
  main_m3: Matrix;
  main_m4: Matrix;
begin
  main_m := make_matrix([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
  writeln(matrix_to_string(main_m));
  writeln(list_list_real_to_str(matrix_columns(main_m)));
  writeln((IntToStr(main_m.rows) + ',') + IntToStr(main_m.cols));
  writeln(LowerCase(BoolToStr(matrix_is_invertible(main_m), true)));
  writeln(matrix_to_string(matrix_identity(main_m)));
  writeln(FloatToStr(matrix_determinant(main_m)));
  writeln(matrix_to_string(matrix_minors(main_m)));
  writeln(matrix_to_string(matrix_cofactors(main_m)));
  writeln(matrix_to_string(matrix_adjugate(main_m)));
  main_m2 := matrix_mul_scalar(main_m, 3);
  writeln(matrix_to_string(main_m2));
  writeln(matrix_to_string(matrix_add(main_m, main_m2)));
  writeln(matrix_to_string(matrix_sub(main_m, main_m2)));
  writeln(matrix_to_string(matrix_pow(main_m, 3)));
  main_m3 := matrix_add_row(main_m, [10, 11, 12]);
  writeln(matrix_to_string(main_m3));
  main_m4 := matrix_add_column(main_m2, [8, 16, 32]);
  writeln(matrix_to_string(matrix_mul(main_m3, main_m4)));
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
