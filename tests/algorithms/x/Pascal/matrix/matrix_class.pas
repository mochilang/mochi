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
  b: Matrix;
  s: real;
  r: integer;
  m: Matrix;
  c: integer;
  row: RealArray;
  values: RealArrayArray;
  col: RealArray;
  p: integer;
  a: Matrix;
function makeMatrix(data: RealArrayArray; rows: integer; cols: integer): Matrix; forward;
function make_matrix(values: RealArrayArray): Matrix; forward;
function matrix_columns(m: Matrix): RealArrayArray; forward;
function matrix_identity(m: Matrix): Matrix; forward;
function matrix_minor(m: Matrix; r: integer; c: integer): real; forward;
function matrix_cofactor(m: Matrix; r: integer; c: integer): real; forward;
function matrix_minors(m: Matrix): Matrix; forward;
function matrix_cofactors(m: Matrix): Matrix; forward;
function matrix_determinant(m: Matrix): real; forward;
function matrix_is_invertible(m: Matrix): boolean; forward;
function matrix_adjugate(m: Matrix): Matrix; forward;
function matrix_inverse(m: Matrix): Matrix; forward;
function matrix_add_row(m: Matrix; row: RealArray): Matrix; forward;
function matrix_add_column(m: Matrix; col: RealArray): Matrix; forward;
function matrix_mul_scalar(m: Matrix; s: real): Matrix; forward;
function matrix_neg(m: Matrix): Matrix; forward;
function matrix_add(a: Matrix; b: Matrix): Matrix; forward;
function matrix_sub(a: Matrix; b: Matrix): Matrix; forward;
function matrix_dot(row: RealArray; col: RealArray): real; forward;
function matrix_mul(a: Matrix; b: Matrix): Matrix; forward;
function matrix_pow(m: Matrix; p: integer): Matrix; forward;
function matrix_to_string(m: Matrix): string; forward;
procedure main(); forward;
function makeMatrix(data: RealArrayArray; rows: integer; cols: integer): Matrix;
begin
  Result.data := data;
  Result.rows := rows;
  Result.cols := cols;
end;
function make_matrix(values: RealArrayArray): Matrix;
var
  make_matrix_r: integer;
  make_matrix_c: integer;
  make_matrix_i: integer;
begin
  make_matrix_r := Length(values);
  if make_matrix_r = 0 then begin
  exit(makeMatrix([], 0, 0));
end;
  make_matrix_c := Length(values[0]);
  make_matrix_i := 0;
  while make_matrix_i < make_matrix_r do begin
  if Length(values[make_matrix_i]) <> make_matrix_c then begin
  exit(makeMatrix([], 0, 0));
end;
  make_matrix_i := make_matrix_i + 1;
end;
  exit(makeMatrix(values, make_matrix_r, make_matrix_c));
end;
function matrix_columns(m: Matrix): RealArrayArray;
var
  matrix_columns_cols: array of RealArray;
  matrix_columns_j: integer;
  matrix_columns_col: array of real;
  matrix_columns_i: integer;
begin
  matrix_columns_cols := [];
  matrix_columns_j := 0;
  while matrix_columns_j < m.cols do begin
  matrix_columns_col := [];
  matrix_columns_i := 0;
  while matrix_columns_i < m.rows do begin
  matrix_columns_col := concat(matrix_columns_col, [m.data[matrix_columns_i][matrix_columns_j]]);
  matrix_columns_i := matrix_columns_i + 1;
end;
  matrix_columns_cols := concat(matrix_columns_cols, [matrix_columns_col]);
  matrix_columns_j := matrix_columns_j + 1;
end;
  exit(matrix_columns_cols);
end;
function matrix_identity(m: Matrix): Matrix;
var
  matrix_identity_vals: array of RealArray;
  matrix_identity_i: integer;
  matrix_identity_row: array of real;
  matrix_identity_j: integer;
  matrix_identity_v: real;
begin
  matrix_identity_vals := [];
  matrix_identity_i := 0;
  while matrix_identity_i < m.rows do begin
  matrix_identity_row := [];
  matrix_identity_j := 0;
  while matrix_identity_j < m.cols do begin
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
  exit(makeMatrix(matrix_identity_vals, m.rows, m.cols));
end;
function matrix_minor(m: Matrix; r: integer; c: integer): real;
var
  matrix_minor_vals: array of RealArray;
  matrix_minor_i: integer;
  matrix_minor_row: array of real;
  matrix_minor_j: integer;
  matrix_minor_sub: Matrix;
begin
  matrix_minor_vals := [];
  matrix_minor_i := 0;
  while matrix_minor_i < m.rows do begin
  if matrix_minor_i <> r then begin
  matrix_minor_row := [];
  matrix_minor_j := 0;
  while matrix_minor_j < m.cols do begin
  if matrix_minor_j <> c then begin
  matrix_minor_row := concat(matrix_minor_row, [m.data[matrix_minor_i][matrix_minor_j]]);
end;
  matrix_minor_j := matrix_minor_j + 1;
end;
  matrix_minor_vals := concat(matrix_minor_vals, [matrix_minor_row]);
end;
  matrix_minor_i := matrix_minor_i + 1;
end;
  matrix_minor_sub := makeMatrix(matrix_minor_vals, m.rows - 1, m.cols - 1);
  exit(matrix_determinant(matrix_minor_sub));
end;
function matrix_cofactor(m: Matrix; r: integer; c: integer): real;
var
  matrix_cofactor_minor: real;
begin
  matrix_cofactor_minor := matrix_minor(m, r, c);
  if ((r + c) mod 2) = 0 then begin
  exit(matrix_cofactor_minor);
end;
  exit(-1 * matrix_cofactor_minor);
end;
function matrix_minors(m: Matrix): Matrix;
var
  matrix_minors_vals: array of RealArray;
  matrix_minors_i: integer;
  matrix_minors_row: array of real;
  matrix_minors_j: integer;
begin
  matrix_minors_vals := [];
  matrix_minors_i := 0;
  while matrix_minors_i < m.rows do begin
  matrix_minors_row := [];
  matrix_minors_j := 0;
  while matrix_minors_j < m.cols do begin
  matrix_minors_row := concat(matrix_minors_row, [matrix_minor(m, matrix_minors_i, matrix_minors_j)]);
  matrix_minors_j := matrix_minors_j + 1;
end;
  matrix_minors_vals := concat(matrix_minors_vals, [matrix_minors_row]);
  matrix_minors_i := matrix_minors_i + 1;
end;
  exit(makeMatrix(matrix_minors_vals, m.rows, m.cols));
end;
function matrix_cofactors(m: Matrix): Matrix;
var
  matrix_cofactors_vals: array of RealArray;
  matrix_cofactors_i: integer;
  matrix_cofactors_row: array of real;
  matrix_cofactors_j: integer;
begin
  matrix_cofactors_vals := [];
  matrix_cofactors_i := 0;
  while matrix_cofactors_i < m.rows do begin
  matrix_cofactors_row := [];
  matrix_cofactors_j := 0;
  while matrix_cofactors_j < m.cols do begin
  matrix_cofactors_row := concat(matrix_cofactors_row, [matrix_cofactor(m, matrix_cofactors_i, matrix_cofactors_j)]);
  matrix_cofactors_j := matrix_cofactors_j + 1;
end;
  matrix_cofactors_vals := concat(matrix_cofactors_vals, [matrix_cofactors_row]);
  matrix_cofactors_i := matrix_cofactors_i + 1;
end;
  exit(makeMatrix(matrix_cofactors_vals, m.rows, m.cols));
end;
function matrix_determinant(m: Matrix): real;
var
  matrix_determinant_sum: real;
  matrix_determinant_j: integer;
begin
  if m.rows <> m.cols then begin
  exit(0);
end;
  if m.rows = 0 then begin
  exit(0);
end;
  if m.rows = 1 then begin
  exit(m.data[0][0]);
end;
  if m.rows = 2 then begin
  exit((m.data[0][0] * m.data[1][1]) - (m.data[0][1] * m.data[1][0]));
end;
  matrix_determinant_sum := 0;
  matrix_determinant_j := 0;
  while matrix_determinant_j < m.cols do begin
  matrix_determinant_sum := matrix_determinant_sum + (m.data[0][matrix_determinant_j] * matrix_cofactor(m, 0, matrix_determinant_j));
  matrix_determinant_j := matrix_determinant_j + 1;
end;
  exit(matrix_determinant_sum);
end;
function matrix_is_invertible(m: Matrix): boolean;
begin
  exit(matrix_determinant(m) <> 0);
end;
function matrix_adjugate(m: Matrix): Matrix;
var
  matrix_adjugate_cof: Matrix;
  matrix_adjugate_vals: array of RealArray;
  matrix_adjugate_i: integer;
  matrix_adjugate_row: array of real;
  matrix_adjugate_j: integer;
begin
  matrix_adjugate_cof := matrix_cofactors(m);
  matrix_adjugate_vals := [];
  matrix_adjugate_i := 0;
  while matrix_adjugate_i < m.rows do begin
  matrix_adjugate_row := [];
  matrix_adjugate_j := 0;
  while matrix_adjugate_j < m.cols do begin
  matrix_adjugate_row := concat(matrix_adjugate_row, [matrix_adjugate_cof.data[matrix_adjugate_j][matrix_adjugate_i]]);
  matrix_adjugate_j := matrix_adjugate_j + 1;
end;
  matrix_adjugate_vals := concat(matrix_adjugate_vals, [matrix_adjugate_row]);
  matrix_adjugate_i := matrix_adjugate_i + 1;
end;
  exit(makeMatrix(matrix_adjugate_vals, m.rows, m.cols));
end;
function matrix_inverse(m: Matrix): Matrix;
var
  matrix_inverse_det: real;
  matrix_inverse_adj: Matrix;
begin
  matrix_inverse_det := matrix_determinant(m);
  if matrix_inverse_det = 0 then begin
  exit(makeMatrix([], 0, 0));
end;
  matrix_inverse_adj := matrix_adjugate(m);
  exit(matrix_mul_scalar(matrix_inverse_adj, 1 / matrix_inverse_det));
end;
function matrix_add_row(m: Matrix; row: RealArray): Matrix;
var
  matrix_add_row_newData: array of RealArray;
begin
  matrix_add_row_newData := m.data;
  matrix_add_row_newData := concat(matrix_add_row_newData, [row]);
  exit(makeMatrix(matrix_add_row_newData, m.rows + 1, m.cols));
end;
function matrix_add_column(m: Matrix; col: RealArray): Matrix;
var
  matrix_add_column_newData: array of RealArray;
  matrix_add_column_i: integer;
begin
  matrix_add_column_newData := [];
  matrix_add_column_i := 0;
  while matrix_add_column_i < m.rows do begin
  matrix_add_column_newData := concat(matrix_add_column_newData, [concat(m.data[matrix_add_column_i], [col[matrix_add_column_i]])]);
  matrix_add_column_i := matrix_add_column_i + 1;
end;
  exit(makeMatrix(matrix_add_column_newData, m.rows, m.cols + 1));
end;
function matrix_mul_scalar(m: Matrix; s: real): Matrix;
var
  matrix_mul_scalar_vals: array of RealArray;
  matrix_mul_scalar_i: integer;
  matrix_mul_scalar_row: array of real;
  matrix_mul_scalar_j: integer;
begin
  matrix_mul_scalar_vals := [];
  matrix_mul_scalar_i := 0;
  while matrix_mul_scalar_i < m.rows do begin
  matrix_mul_scalar_row := [];
  matrix_mul_scalar_j := 0;
  while matrix_mul_scalar_j < m.cols do begin
  matrix_mul_scalar_row := concat(matrix_mul_scalar_row, [m.data[matrix_mul_scalar_i][matrix_mul_scalar_j] * s]);
  matrix_mul_scalar_j := matrix_mul_scalar_j + 1;
end;
  matrix_mul_scalar_vals := concat(matrix_mul_scalar_vals, [matrix_mul_scalar_row]);
  matrix_mul_scalar_i := matrix_mul_scalar_i + 1;
end;
  exit(makeMatrix(matrix_mul_scalar_vals, m.rows, m.cols));
end;
function matrix_neg(m: Matrix): Matrix;
begin
  exit(matrix_mul_scalar(m, -1));
end;
function matrix_add(a: Matrix; b: Matrix): Matrix;
var
  matrix_add_vals: array of RealArray;
  matrix_add_i: integer;
  matrix_add_row_var: array of real;
  matrix_add_j: integer;
begin
  if (a.rows <> b.rows) or (a.cols <> b.cols) then begin
  exit(makeMatrix([], 0, 0));
end;
  matrix_add_vals := [];
  matrix_add_i := 0;
  while matrix_add_i < a.rows do begin
  matrix_add_row_var := [];
  matrix_add_j := 0;
  while matrix_add_j < a.cols do begin
  matrix_add_row_var := concat(matrix_add_row_var, [a.data[matrix_add_i][matrix_add_j] + b.data[matrix_add_i][matrix_add_j]]);
  matrix_add_j := matrix_add_j + 1;
end;
  matrix_add_vals := concat(matrix_add_vals, [matrix_add_row_var]);
  matrix_add_i := matrix_add_i + 1;
end;
  exit(makeMatrix(matrix_add_vals, a.rows, a.cols));
end;
function matrix_sub(a: Matrix; b: Matrix): Matrix;
var
  matrix_sub_vals: array of RealArray;
  matrix_sub_i: integer;
  matrix_sub_row: array of real;
  matrix_sub_j: integer;
begin
  if (a.rows <> b.rows) or (a.cols <> b.cols) then begin
  exit(makeMatrix([], 0, 0));
end;
  matrix_sub_vals := [];
  matrix_sub_i := 0;
  while matrix_sub_i < a.rows do begin
  matrix_sub_row := [];
  matrix_sub_j := 0;
  while matrix_sub_j < a.cols do begin
  matrix_sub_row := concat(matrix_sub_row, [a.data[matrix_sub_i][matrix_sub_j] - b.data[matrix_sub_i][matrix_sub_j]]);
  matrix_sub_j := matrix_sub_j + 1;
end;
  matrix_sub_vals := concat(matrix_sub_vals, [matrix_sub_row]);
  matrix_sub_i := matrix_sub_i + 1;
end;
  exit(makeMatrix(matrix_sub_vals, a.rows, a.cols));
end;
function matrix_dot(row: RealArray; col: RealArray): real;
var
  matrix_dot_sum: real;
  matrix_dot_i: integer;
begin
  matrix_dot_sum := 0;
  matrix_dot_i := 0;
  while matrix_dot_i < Length(row) do begin
  matrix_dot_sum := matrix_dot_sum + (row[matrix_dot_i] * col[matrix_dot_i]);
  matrix_dot_i := matrix_dot_i + 1;
end;
  exit(matrix_dot_sum);
end;
function matrix_mul(a: Matrix; b: Matrix): Matrix;
var
  matrix_mul_bcols: RealArrayArray;
  matrix_mul_vals: array of RealArray;
  matrix_mul_i: integer;
  matrix_mul_row: array of real;
  matrix_mul_j: integer;
begin
  if a.cols <> b.rows then begin
  exit(makeMatrix([], 0, 0));
end;
  matrix_mul_bcols := matrix_columns(b);
  matrix_mul_vals := [];
  matrix_mul_i := 0;
  while matrix_mul_i < a.rows do begin
  matrix_mul_row := [];
  matrix_mul_j := 0;
  while matrix_mul_j < b.cols do begin
  matrix_mul_row := concat(matrix_mul_row, [matrix_dot(a.data[matrix_mul_i], matrix_mul_bcols[matrix_mul_j])]);
  matrix_mul_j := matrix_mul_j + 1;
end;
  matrix_mul_vals := concat(matrix_mul_vals, [matrix_mul_row]);
  matrix_mul_i := matrix_mul_i + 1;
end;
  exit(makeMatrix(matrix_mul_vals, a.rows, b.cols));
end;
function matrix_pow(m: Matrix; p: integer): Matrix;
var
  matrix_pow_result_: Matrix;
  matrix_pow_i: integer;
begin
  if p = 0 then begin
  exit(matrix_identity(m));
end;
  if p < 0 then begin
  if matrix_is_invertible(m) then begin
  exit(matrix_pow(matrix_inverse(m), -p));
end;
  exit(makeMatrix([], 0, 0));
end;
  matrix_pow_result_ := m;
  matrix_pow_i := 1;
  while matrix_pow_i < p do begin
  matrix_pow_result_ := matrix_mul(matrix_pow_result_, m);
  matrix_pow_i := matrix_pow_i + 1;
end;
  exit(matrix_pow_result_);
end;
function matrix_to_string(m: Matrix): string;
var
  matrix_to_string_s: string;
  matrix_to_string_i: integer;
  matrix_to_string_j: integer;
begin
  if m.rows = 0 then begin
  exit('[]');
end;
  matrix_to_string_s := '[';
  matrix_to_string_i := 0;
  while matrix_to_string_i < m.rows do begin
  matrix_to_string_s := matrix_to_string_s + '[';
  matrix_to_string_j := 0;
  while matrix_to_string_j < m.cols do begin
  matrix_to_string_s := matrix_to_string_s + FloatToStr(m.data[matrix_to_string_i][matrix_to_string_j]);
  if matrix_to_string_j < (m.cols - 1) then begin
  matrix_to_string_s := matrix_to_string_s + ' ';
end;
  matrix_to_string_j := matrix_to_string_j + 1;
end;
  matrix_to_string_s := matrix_to_string_s + ']';
  if matrix_to_string_i < (m.rows - 1) then begin
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
