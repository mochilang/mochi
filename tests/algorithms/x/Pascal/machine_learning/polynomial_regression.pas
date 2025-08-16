{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  xs: array of real;
  ys: array of real;
  i: integer;
  x: real;
  X_54: RealArrayArray;
  Xt: RealArrayArray;
  XtX: RealArrayArray;
  Xty: RealArray;
  coeffs: RealArray;
  v: RealArray;
  A: RealArrayArray;
  matrix: RealArrayArray;
  B: RealArrayArray;
  b: RealArray;
  degree: integer;
function design_matrix(xs: RealArray; degree: integer): RealArrayArray; forward;
function transpose(matrix: RealArrayArray): RealArrayArray; forward;
function matmul(A: RealArrayArray; B: RealArrayArray): RealArrayArray; forward;
function matvec_mul(A: RealArrayArray; v: RealArray): RealArray; forward;
function gaussian_elimination(A: RealArrayArray; b: RealArray): RealArray; forward;
function predict(xs: RealArray; coeffs: RealArray): RealArray; forward;
function design_matrix(xs: RealArray; degree: integer): RealArrayArray;
var
  design_matrix_i: integer;
  design_matrix_matrix: array of RealArray;
  design_matrix_row: array of real;
  design_matrix_j: integer;
  design_matrix_pow: real;
begin
  design_matrix_i := 0;
  design_matrix_matrix := [];
  while design_matrix_i < Length(xs) do begin
  design_matrix_row := [];
  design_matrix_j := 0;
  design_matrix_pow := 1;
  while design_matrix_j <= degree do begin
  design_matrix_row := concat(design_matrix_row, [design_matrix_pow]);
  design_matrix_pow := design_matrix_pow * xs[design_matrix_i];
  design_matrix_j := design_matrix_j + 1;
end;
  design_matrix_matrix := concat(design_matrix_matrix, [design_matrix_row]);
  design_matrix_i := design_matrix_i + 1;
end;
  exit(design_matrix_matrix);
end;
function transpose(matrix: RealArrayArray): RealArrayArray;
var
  transpose_rows: integer;
  transpose_cols: integer;
  transpose_j: integer;
  transpose_result_: array of RealArray;
  transpose_row: array of real;
  transpose_i: integer;
begin
  transpose_rows := Length(matrix);
  transpose_cols := Length(matrix[0]);
  transpose_j := 0;
  transpose_result_ := [];
  while transpose_j < transpose_cols do begin
  transpose_row := [];
  transpose_i := 0;
  while transpose_i < transpose_rows do begin
  transpose_row := concat(transpose_row, [matrix[transpose_i][transpose_j]]);
  transpose_i := transpose_i + 1;
end;
  transpose_result_ := concat(transpose_result_, [transpose_row]);
  transpose_j := transpose_j + 1;
end;
  exit(transpose_result_);
end;
function matmul(A: RealArrayArray; B: RealArrayArray): RealArrayArray;
var
  matmul_n: integer;
  matmul_m: integer;
  matmul_p: integer;
  matmul_i: integer;
  matmul_result_: array of RealArray;
  matmul_row: array of real;
  matmul_k: integer;
  matmul_sum: real;
  matmul_j: integer;
begin
  matmul_n := Length(A);
  matmul_m := Length(A[0]);
  matmul_p := Length(B[0]);
  matmul_i := 0;
  matmul_result_ := [];
  while matmul_i < matmul_n do begin
  matmul_row := [];
  matmul_k := 0;
  while matmul_k < matmul_p do begin
  matmul_sum := 0;
  matmul_j := 0;
  while matmul_j < matmul_m do begin
  matmul_sum := matmul_sum + (A[matmul_i][matmul_j] * B[matmul_j][matmul_k]);
  matmul_j := matmul_j + 1;
end;
  matmul_row := concat(matmul_row, [matmul_sum]);
  matmul_k := matmul_k + 1;
end;
  matmul_result_ := concat(matmul_result_, [matmul_row]);
  matmul_i := matmul_i + 1;
end;
  exit(matmul_result_);
end;
function matvec_mul(A: RealArrayArray; v: RealArray): RealArray;
var
  matvec_mul_n: integer;
  matvec_mul_m: integer;
  matvec_mul_i: integer;
  matvec_mul_result_: array of real;
  matvec_mul_sum: real;
  matvec_mul_j: integer;
begin
  matvec_mul_n := Length(A);
  matvec_mul_m := Length(A[0]);
  matvec_mul_i := 0;
  matvec_mul_result_ := [];
  while matvec_mul_i < matvec_mul_n do begin
  matvec_mul_sum := 0;
  matvec_mul_j := 0;
  while matvec_mul_j < matvec_mul_m do begin
  matvec_mul_sum := matvec_mul_sum + (A[matvec_mul_i][matvec_mul_j] * v[matvec_mul_j]);
  matvec_mul_j := matvec_mul_j + 1;
end;
  matvec_mul_result_ := concat(matvec_mul_result_, [matvec_mul_sum]);
  matvec_mul_i := matvec_mul_i + 1;
end;
  exit(matvec_mul_result_);
end;
function gaussian_elimination(A: RealArrayArray; b: RealArray): RealArray;
var
  gaussian_elimination_n: integer;
  gaussian_elimination_M: array of RealArray;
  gaussian_elimination_i: integer;
  gaussian_elimination_k: integer;
  gaussian_elimination_j: integer;
  gaussian_elimination_factor: real;
  gaussian_elimination_rowj: array of real;
  gaussian_elimination_rowk: array of real;
  gaussian_elimination_l: integer;
  gaussian_elimination_x: array of real;
  gaussian_elimination_t: integer;
  gaussian_elimination_i2: integer;
  gaussian_elimination_sum: real;
  gaussian_elimination_j2: integer;
begin
  gaussian_elimination_n := Length(A);
  gaussian_elimination_M := [];
  gaussian_elimination_i := 0;
  while gaussian_elimination_i < gaussian_elimination_n do begin
  gaussian_elimination_M := concat(gaussian_elimination_M, [concat(A[gaussian_elimination_i], [b[gaussian_elimination_i]])]);
  gaussian_elimination_i := gaussian_elimination_i + 1;
end;
  gaussian_elimination_k := 0;
  while gaussian_elimination_k < gaussian_elimination_n do begin
  gaussian_elimination_j := gaussian_elimination_k + 1;
  while gaussian_elimination_j < gaussian_elimination_n do begin
  gaussian_elimination_factor := gaussian_elimination_M[gaussian_elimination_j][gaussian_elimination_k] / gaussian_elimination_M[gaussian_elimination_k][gaussian_elimination_k];
  gaussian_elimination_rowj := gaussian_elimination_M[gaussian_elimination_j];
  gaussian_elimination_rowk := gaussian_elimination_M[gaussian_elimination_k];
  gaussian_elimination_l := gaussian_elimination_k;
  while gaussian_elimination_l <= gaussian_elimination_n do begin
  gaussian_elimination_rowj[gaussian_elimination_l] := gaussian_elimination_rowj[gaussian_elimination_l] - (gaussian_elimination_factor * gaussian_elimination_rowk[gaussian_elimination_l]);
  gaussian_elimination_l := gaussian_elimination_l + 1;
end;
  gaussian_elimination_M[gaussian_elimination_j] := gaussian_elimination_rowj;
  gaussian_elimination_j := gaussian_elimination_j + 1;
end;
  gaussian_elimination_k := gaussian_elimination_k + 1;
end;
  gaussian_elimination_x := [];
  gaussian_elimination_t := 0;
  while gaussian_elimination_t < gaussian_elimination_n do begin
  gaussian_elimination_x := concat(gaussian_elimination_x, [0]);
  gaussian_elimination_t := gaussian_elimination_t + 1;
end;
  gaussian_elimination_i2 := gaussian_elimination_n - 1;
  while gaussian_elimination_i2 >= 0 do begin
  gaussian_elimination_sum := gaussian_elimination_M[gaussian_elimination_i2][gaussian_elimination_n];
  gaussian_elimination_j2 := gaussian_elimination_i2 + 1;
  while gaussian_elimination_j2 < gaussian_elimination_n do begin
  gaussian_elimination_sum := gaussian_elimination_sum - (gaussian_elimination_M[gaussian_elimination_i2][gaussian_elimination_j2] * gaussian_elimination_x[gaussian_elimination_j2]);
  gaussian_elimination_j2 := gaussian_elimination_j2 + 1;
end;
  gaussian_elimination_x[gaussian_elimination_i2] := gaussian_elimination_sum / gaussian_elimination_M[gaussian_elimination_i2][gaussian_elimination_i2];
  gaussian_elimination_i2 := gaussian_elimination_i2 - 1;
end;
  exit(gaussian_elimination_x);
end;
function predict(xs: RealArray; coeffs: RealArray): RealArray;
var
  predict_i: integer;
  predict_result_: array of real;
  predict_x: real;
  predict_j: integer;
  predict_pow: real;
  predict_sum: real;
begin
  predict_i := 0;
  predict_result_ := [];
  while predict_i < Length(xs) do begin
  predict_x := xs[predict_i];
  predict_j := 0;
  predict_pow := 1;
  predict_sum := 0;
  while predict_j < Length(coeffs) do begin
  predict_sum := predict_sum + (coeffs[predict_j] * predict_pow);
  predict_pow := predict_pow * predict_x;
  predict_j := predict_j + 1;
end;
  predict_result_ := concat(predict_result_, [predict_sum]);
  predict_i := predict_i + 1;
end;
  exit(predict_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  xs := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  ys := [];
  i := 0;
  while i < Length(xs) do begin
  x := xs[i];
  ys := concat(ys, [((((x * x) * x) - ((2 * x) * x)) + (3 * x)) - 5]);
  i := i + 1;
end;
  X_54 := design_matrix(xs, 3);
  Xt := transpose(X_54);
  XtX := matmul(Xt, X_54);
  Xty := matvec_mul(Xt, ys);
  coeffs := gaussian_elimination(XtX, Xty);
  writeln(list_real_to_str(coeffs));
  writeln(list_real_to_str(predict([-1], coeffs)));
  writeln(list_real_to_str(predict([-2], coeffs)));
  writeln(list_real_to_str(predict([6], coeffs)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
