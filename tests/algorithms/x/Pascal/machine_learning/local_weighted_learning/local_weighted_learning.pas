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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  x_train: array of RealArray;
  y_train: array of real;
  preds: RealArray;
  a: RealArrayArray;
  x: real;
  tau: real;
  b: RealArrayArray;
  mat: RealArrayArray;
  point: RealArray;
function expApprox(x: real): real; forward;
function transpose(mat: RealArrayArray): RealArrayArray; forward;
function matMul(a: RealArrayArray; b: RealArrayArray): RealArrayArray; forward;
function matInv(mat: RealArrayArray): RealArrayArray; forward;
function weight_matrix(point: RealArray; x_train: RealArrayArray; tau: real): RealArrayArray; forward;
function local_weight(point: RealArray; x_train: RealArrayArray; y_train: RealArray; tau: real): RealArrayArray; forward;
function local_weight_regression(x_train: RealArrayArray; y_train: RealArray; tau: real): RealArray; forward;
function expApprox(x: real): real;
var
  expApprox_half: real;
  expApprox_sum: real;
  expApprox_term: real;
  expApprox_n: integer;
begin
  if x < 0 then begin
  exit(1 / expApprox(-x));
end;
  if x > 1 then begin
  expApprox_half := expApprox(x / 2);
  exit(expApprox_half * expApprox_half);
end;
  expApprox_sum := 1;
  expApprox_term := 1;
  expApprox_n := 1;
  while expApprox_n < 20 do begin
  expApprox_term := (expApprox_term * x) / Double(expApprox_n);
  expApprox_sum := expApprox_sum + expApprox_term;
  expApprox_n := expApprox_n + 1;
end;
  exit(expApprox_sum);
end;
function transpose(mat: RealArrayArray): RealArrayArray;
var
  transpose_rows: integer;
  transpose_cols: integer;
  transpose_res: array of RealArray;
  transpose_i: integer;
  transpose_row: array of real;
  transpose_j: integer;
begin
  transpose_rows := Length(mat);
  transpose_cols := Length(mat[0]);
  transpose_res := [];
  transpose_i := 0;
  while transpose_i < transpose_cols do begin
  transpose_row := [];
  transpose_j := 0;
  while transpose_j < transpose_rows do begin
  transpose_row := concat(transpose_row, [mat[transpose_j][transpose_i]]);
  transpose_j := transpose_j + 1;
end;
  transpose_res := concat(transpose_res, [transpose_row]);
  transpose_i := transpose_i + 1;
end;
  exit(transpose_res);
end;
function matMul(a: RealArrayArray; b: RealArrayArray): RealArrayArray;
var
  matMul_a_rows: integer;
  matMul_a_cols: integer;
  matMul_b_cols: integer;
  matMul_res: array of RealArray;
  matMul_i: integer;
  matMul_row: array of real;
  matMul_j: integer;
  matMul_sum: real;
  matMul_k: integer;
begin
  matMul_a_rows := Length(a);
  matMul_a_cols := Length(a[0]);
  matMul_b_cols := Length(b[0]);
  matMul_res := [];
  matMul_i := 0;
  while matMul_i < matMul_a_rows do begin
  matMul_row := [];
  matMul_j := 0;
  while matMul_j < matMul_b_cols do begin
  matMul_sum := 0;
  matMul_k := 0;
  while matMul_k < matMul_a_cols do begin
  matMul_sum := matMul_sum + (a[matMul_i][matMul_k] * b[matMul_k][matMul_j]);
  matMul_k := matMul_k + 1;
end;
  matMul_row := concat(matMul_row, [matMul_sum]);
  matMul_j := matMul_j + 1;
end;
  matMul_res := concat(matMul_res, [matMul_row]);
  matMul_i := matMul_i + 1;
end;
  exit(matMul_res);
end;
function matInv(mat: RealArrayArray): RealArrayArray;
var
  matInv_n: integer;
  matInv_aug: array of RealArray;
  matInv_i: integer;
  matInv_row: array of real;
  matInv_j: integer;
  matInv_col: integer;
  matInv_pivot: real;
  matInv_r: integer;
  matInv_factor: real;
  matInv_inv: array of RealArray;
begin
  matInv_n := Length(mat);
  matInv_aug := [];
  matInv_i := 0;
  while matInv_i < matInv_n do begin
  matInv_row := [];
  matInv_j := 0;
  while matInv_j < matInv_n do begin
  matInv_row := concat(matInv_row, [mat[matInv_i][matInv_j]]);
  matInv_j := matInv_j + 1;
end;
  matInv_j := 0;
  while matInv_j < matInv_n do begin
  if matInv_i = matInv_j then begin
  matInv_row := concat(matInv_row, [1]);
end else begin
  matInv_row := concat(matInv_row, [0]);
end;
  matInv_j := matInv_j + 1;
end;
  matInv_aug := concat(matInv_aug, [matInv_row]);
  matInv_i := matInv_i + 1;
end;
  matInv_col := 0;
  while matInv_col < matInv_n do begin
  matInv_pivot := matInv_aug[matInv_col][matInv_col];
  if matInv_pivot = 0 then begin
  panic('Matrix is singular');
end;
  matInv_j := 0;
  while matInv_j < (2 * matInv_n) do begin
  matInv_aug[matInv_col][matInv_j] := matInv_aug[matInv_col][matInv_j] / matInv_pivot;
  matInv_j := matInv_j + 1;
end;
  matInv_r := 0;
  while matInv_r < matInv_n do begin
  if matInv_r <> matInv_col then begin
  matInv_factor := matInv_aug[matInv_r][matInv_col];
  matInv_j := 0;
  while matInv_j < (2 * matInv_n) do begin
  matInv_aug[matInv_r][matInv_j] := matInv_aug[matInv_r][matInv_j] - (matInv_factor * matInv_aug[matInv_col][matInv_j]);
  matInv_j := matInv_j + 1;
end;
end;
  matInv_r := matInv_r + 1;
end;
  matInv_col := matInv_col + 1;
end;
  matInv_inv := [];
  matInv_i := 0;
  while matInv_i < matInv_n do begin
  matInv_row := [];
  matInv_j := 0;
  while matInv_j < matInv_n do begin
  matInv_row := concat(matInv_row, [matInv_aug[matInv_i][matInv_j + matInv_n]]);
  matInv_j := matInv_j + 1;
end;
  matInv_inv := concat(matInv_inv, [matInv_row]);
  matInv_i := matInv_i + 1;
end;
  exit(matInv_inv);
end;
function weight_matrix(point: RealArray; x_train: RealArrayArray; tau: real): RealArrayArray;
var
  weight_matrix_m: integer;
  weight_matrix_weights: array of RealArray;
  weight_matrix_i: integer;
  weight_matrix_row: array of real;
  weight_matrix_j: integer;
  weight_matrix_diff_sq: real;
  weight_matrix_k: integer;
  weight_matrix_diff: real;
begin
  weight_matrix_m := Length(x_train);
  weight_matrix_weights := [];
  weight_matrix_i := 0;
  while weight_matrix_i < weight_matrix_m do begin
  weight_matrix_row := [];
  weight_matrix_j := 0;
  while weight_matrix_j < weight_matrix_m do begin
  if weight_matrix_i = weight_matrix_j then begin
  weight_matrix_row := concat(weight_matrix_row, [1]);
end else begin
  weight_matrix_row := concat(weight_matrix_row, [0]);
end;
  weight_matrix_j := weight_matrix_j + 1;
end;
  weight_matrix_weights := concat(weight_matrix_weights, [weight_matrix_row]);
  weight_matrix_i := weight_matrix_i + 1;
end;
  weight_matrix_j := 0;
  while weight_matrix_j < weight_matrix_m do begin
  weight_matrix_diff_sq := 0;
  weight_matrix_k := 0;
  while weight_matrix_k < Length(point) do begin
  weight_matrix_diff := point[weight_matrix_k] - x_train[weight_matrix_j][weight_matrix_k];
  weight_matrix_diff_sq := weight_matrix_diff_sq + (weight_matrix_diff * weight_matrix_diff);
  weight_matrix_k := weight_matrix_k + 1;
end;
  weight_matrix_weights[weight_matrix_j][weight_matrix_j] := expApprox(-weight_matrix_diff_sq / ((2 * tau) * tau));
  weight_matrix_j := weight_matrix_j + 1;
end;
  exit(weight_matrix_weights);
end;
function local_weight(point: RealArray; x_train: RealArrayArray; y_train: RealArray; tau: real): RealArrayArray;
var
  local_weight_w: RealArrayArray;
  local_weight_x_t: RealArrayArray;
  local_weight_x_t_w: RealArrayArray;
  local_weight_x_t_w_x: RealArrayArray;
  local_weight_inv_part: RealArrayArray;
  local_weight_y_col: array of RealArray;
  local_weight_i: integer;
  local_weight_x_t_w_y: RealArrayArray;
begin
  local_weight_w := weight_matrix(point, x_train, tau);
  local_weight_x_t := transpose(x_train);
  local_weight_x_t_w := matMul(local_weight_x_t, local_weight_w);
  local_weight_x_t_w_x := matMul(local_weight_x_t_w, x_train);
  local_weight_inv_part := matInv(local_weight_x_t_w_x);
  local_weight_y_col := [];
  local_weight_i := 0;
  while local_weight_i < Length(y_train) do begin
  local_weight_y_col := concat(local_weight_y_col, [[y_train[local_weight_i]]]);
  local_weight_i := local_weight_i + 1;
end;
  local_weight_x_t_w_y := matMul(local_weight_x_t_w, local_weight_y_col);
  exit(matMul(local_weight_inv_part, local_weight_x_t_w_y));
end;
function local_weight_regression(x_train: RealArrayArray; y_train: RealArray; tau: real): RealArray;
var
  local_weight_regression_m: integer;
  local_weight_regression_preds: array of real;
  local_weight_regression_i: integer;
  local_weight_regression_theta: RealArrayArray;
  local_weight_regression_weights_vec: array of real;
  local_weight_regression_k: integer;
  local_weight_regression_pred: real;
  local_weight_regression_j: integer;
begin
  local_weight_regression_m := Length(x_train);
  local_weight_regression_preds := [];
  local_weight_regression_i := 0;
  while local_weight_regression_i < local_weight_regression_m do begin
  local_weight_regression_theta := local_weight(x_train[local_weight_regression_i], x_train, y_train, tau);
  local_weight_regression_weights_vec := [];
  local_weight_regression_k := 0;
  while local_weight_regression_k < Length(local_weight_regression_theta) do begin
  local_weight_regression_weights_vec := concat(local_weight_regression_weights_vec, [local_weight_regression_theta[local_weight_regression_k][0]]);
  local_weight_regression_k := local_weight_regression_k + 1;
end;
  local_weight_regression_pred := 0;
  local_weight_regression_j := 0;
  while local_weight_regression_j < Length(x_train[local_weight_regression_i]) do begin
  local_weight_regression_pred := local_weight_regression_pred + (x_train[local_weight_regression_i][local_weight_regression_j] * local_weight_regression_weights_vec[local_weight_regression_j]);
  local_weight_regression_j := local_weight_regression_j + 1;
end;
  local_weight_regression_preds := concat(local_weight_regression_preds, [local_weight_regression_pred]);
  local_weight_regression_i := local_weight_regression_i + 1;
end;
  exit(local_weight_regression_preds);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  x_train := [[16.99, 10.34], [21.01, 23.68], [24.59, 25.69]];
  y_train := [1.01, 1.66, 3.5];
  preds := local_weight_regression(x_train, y_train, 0.6);
  json(preds);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
