{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
type PCAResult = record
  transformed: array of RealArray;
  variance_ratio: array of real;
end;
type Eigen = record
  values: array of real;
  vectors: array of RealArray;
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
procedure show_list_real(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
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
  data: array of RealArray;
  result_: PCAResult;
  idx: integer;
  b: RealArrayArray;
  xs: RealArray;
  vec: RealArray;
  a: RealArrayArray;
  matrix: RealArrayArray;
  n_components: integer;
  x: real;
function makeEigen(values: RealArray; vectors: RealArrayArray): Eigen; forward;
function makePCAResult(transformed: RealArrayArray; variance_ratio: RealArray): PCAResult; forward;
function sqrt(x: real): real; forward;
function mean(xs: RealArray): real; forward;
function standardize(data: RealArrayArray): RealArrayArray; forward;
function covariance_matrix(data: RealArrayArray): RealArrayArray; forward;
function normalize(vec: RealArray): RealArray; forward;
function eigen_decomposition_2x2(matrix: RealArrayArray): Eigen; forward;
function transpose(matrix: RealArrayArray): RealArrayArray; forward;
function matrix_multiply(a: RealArrayArray; b: RealArrayArray): RealArrayArray; forward;
function apply_pca(data: RealArrayArray; n_components: integer): PCAResult; forward;
function makeEigen(values: RealArray; vectors: RealArrayArray): Eigen;
begin
  Result.values := values;
  Result.vectors := vectors;
end;
function makePCAResult(transformed: RealArrayArray; variance_ratio: RealArray): PCAResult;
begin
  Result.transformed := transformed;
  Result.variance_ratio := variance_ratio;
end;
function sqrt(x: real): real;
var
  sqrt_guess: real;
  sqrt_i: integer;
begin
  sqrt_guess := IfThen(x > 1, x / 2, 1);
  sqrt_i := 0;
  while sqrt_i < 20 do begin
  sqrt_guess := 0.5 * (sqrt_guess + (x / sqrt_guess));
  sqrt_i := sqrt_i + 1;
end;
  exit(sqrt_guess);
end;
function mean(xs: RealArray): real;
var
  mean_sum: real;
  mean_i: integer;
begin
  mean_sum := 0;
  mean_i := 0;
  while mean_i < Length(xs) do begin
  mean_sum := mean_sum + xs[mean_i];
  mean_i := mean_i + 1;
end;
  exit(mean_sum / Length(xs));
end;
function standardize(data: RealArrayArray): RealArrayArray;
var
  standardize_n_samples: integer;
  standardize_n_features: integer;
  standardize_means: array of real;
  standardize_stds: array of real;
  standardize_j: integer;
  standardize_column: array of real;
  standardize_i: integer;
  standardize_m: real;
  standardize_variance: real;
  standardize_k: integer;
  standardize_diff: real;
  standardize_standardized: array of RealArray;
  standardize_r: integer;
  standardize_row: array of real;
  standardize_c: integer;
begin
  standardize_n_samples := Length(data);
  standardize_n_features := Length(data[0]);
  standardize_means := [];
  standardize_stds := [];
  standardize_j := 0;
  while standardize_j < standardize_n_features do begin
  standardize_column := [];
  standardize_i := 0;
  while standardize_i < standardize_n_samples do begin
  standardize_column := concat(standardize_column, [data[standardize_i][standardize_j]]);
  standardize_i := standardize_i + 1;
end;
  standardize_m := mean(standardize_column);
  standardize_means := concat(standardize_means, [standardize_m]);
  standardize_variance := 0;
  standardize_k := 0;
  while standardize_k < standardize_n_samples do begin
  standardize_diff := standardize_column[standardize_k] - standardize_m;
  standardize_variance := standardize_variance + (standardize_diff * standardize_diff);
  standardize_k := standardize_k + 1;
end;
  standardize_stds := concat(standardize_stds, [sqrt(standardize_variance / (standardize_n_samples - 1))]);
  standardize_j := standardize_j + 1;
end;
  standardize_standardized := [];
  standardize_r := 0;
  while standardize_r < standardize_n_samples do begin
  standardize_row := [];
  standardize_c := 0;
  while standardize_c < standardize_n_features do begin
  standardize_row := concat(standardize_row, [(data[standardize_r][standardize_c] - standardize_means[standardize_c]) / standardize_stds[standardize_c]]);
  standardize_c := standardize_c + 1;
end;
  standardize_standardized := concat(standardize_standardized, [standardize_row]);
  standardize_r := standardize_r + 1;
end;
  exit(standardize_standardized);
end;
function covariance_matrix(data: RealArrayArray): RealArrayArray;
var
  covariance_matrix_n_samples: integer;
  covariance_matrix_n_features: integer;
  covariance_matrix_cov: array of RealArray;
  covariance_matrix_i: integer;
  covariance_matrix_row: array of real;
  covariance_matrix_j: integer;
  covariance_matrix_sum: real;
  covariance_matrix_k: integer;
begin
  covariance_matrix_n_samples := Length(data);
  covariance_matrix_n_features := Length(data[0]);
  covariance_matrix_cov := [];
  covariance_matrix_i := 0;
  while covariance_matrix_i < covariance_matrix_n_features do begin
  covariance_matrix_row := [];
  covariance_matrix_j := 0;
  while covariance_matrix_j < covariance_matrix_n_features do begin
  covariance_matrix_sum := 0;
  covariance_matrix_k := 0;
  while covariance_matrix_k < covariance_matrix_n_samples do begin
  covariance_matrix_sum := covariance_matrix_sum + (data[covariance_matrix_k][covariance_matrix_i] * data[covariance_matrix_k][covariance_matrix_j]);
  covariance_matrix_k := covariance_matrix_k + 1;
end;
  covariance_matrix_row := concat(covariance_matrix_row, [covariance_matrix_sum / (covariance_matrix_n_samples - 1)]);
  covariance_matrix_j := covariance_matrix_j + 1;
end;
  covariance_matrix_cov := concat(covariance_matrix_cov, [covariance_matrix_row]);
  covariance_matrix_i := covariance_matrix_i + 1;
end;
  exit(covariance_matrix_cov);
end;
function normalize(vec: RealArray): RealArray;
var
  normalize_sum: real;
  normalize_i: integer;
  normalize_n: real;
  normalize_res: array of real;
  normalize_j: integer;
begin
  normalize_sum := 0;
  normalize_i := 0;
  while normalize_i < Length(vec) do begin
  normalize_sum := normalize_sum + (vec[normalize_i] * vec[normalize_i]);
  normalize_i := normalize_i + 1;
end;
  normalize_n := sqrt(normalize_sum);
  normalize_res := [];
  normalize_j := 0;
  while normalize_j < Length(vec) do begin
  normalize_res := concat(normalize_res, [vec[normalize_j] / normalize_n]);
  normalize_j := normalize_j + 1;
end;
  exit(normalize_res);
end;
function eigen_decomposition_2x2(matrix: RealArrayArray): Eigen;
var
  eigen_decomposition_2x2_a: real;
  eigen_decomposition_2x2_b: real;
  eigen_decomposition_2x2_c: real;
  eigen_decomposition_2x2_diff: real;
  eigen_decomposition_2x2_discriminant: real;
  eigen_decomposition_2x2_lambda1: real;
  eigen_decomposition_2x2_lambda2: real;
  eigen_decomposition_2x2_v1: array of real;
  eigen_decomposition_2x2_v2: array of real;
  eigen_decomposition_2x2_eigenvalues: array of real;
  eigen_decomposition_2x2_eigenvectors: array of RealArray;
  eigen_decomposition_2x2_tmp_val: real;
  eigen_decomposition_2x2_tmp_vec: array of real;
begin
  eigen_decomposition_2x2_a := matrix[0][0];
  eigen_decomposition_2x2_b := matrix[0][1];
  eigen_decomposition_2x2_c := matrix[1][1];
  eigen_decomposition_2x2_diff := eigen_decomposition_2x2_a - eigen_decomposition_2x2_c;
  eigen_decomposition_2x2_discriminant := sqrt((eigen_decomposition_2x2_diff * eigen_decomposition_2x2_diff) + ((4 * eigen_decomposition_2x2_b) * eigen_decomposition_2x2_b));
  eigen_decomposition_2x2_lambda1 := ((eigen_decomposition_2x2_a + eigen_decomposition_2x2_c) + eigen_decomposition_2x2_discriminant) / 2;
  eigen_decomposition_2x2_lambda2 := ((eigen_decomposition_2x2_a + eigen_decomposition_2x2_c) - eigen_decomposition_2x2_discriminant) / 2;
  if eigen_decomposition_2x2_b <> 0 then begin
  eigen_decomposition_2x2_v1 := normalize([eigen_decomposition_2x2_lambda1 - eigen_decomposition_2x2_c, eigen_decomposition_2x2_b]);
  eigen_decomposition_2x2_v2 := normalize([eigen_decomposition_2x2_lambda2 - eigen_decomposition_2x2_c, eigen_decomposition_2x2_b]);
end else begin
  eigen_decomposition_2x2_v1 := [1, 0];
  eigen_decomposition_2x2_v2 := [0, 1];
end;
  eigen_decomposition_2x2_eigenvalues := [eigen_decomposition_2x2_lambda1, eigen_decomposition_2x2_lambda2];
  eigen_decomposition_2x2_eigenvectors := [eigen_decomposition_2x2_v1, eigen_decomposition_2x2_v2];
  if eigen_decomposition_2x2_eigenvalues[0] < eigen_decomposition_2x2_eigenvalues[1] then begin
  eigen_decomposition_2x2_tmp_val := eigen_decomposition_2x2_eigenvalues[0];
  eigen_decomposition_2x2_eigenvalues[0] := eigen_decomposition_2x2_eigenvalues[1];
  eigen_decomposition_2x2_eigenvalues[1] := eigen_decomposition_2x2_tmp_val;
  eigen_decomposition_2x2_tmp_vec := eigen_decomposition_2x2_eigenvectors[0];
  eigen_decomposition_2x2_eigenvectors[0] := eigen_decomposition_2x2_eigenvectors[1];
  eigen_decomposition_2x2_eigenvectors[1] := eigen_decomposition_2x2_tmp_vec;
end;
  exit(makeEigen(eigen_decomposition_2x2_eigenvalues, eigen_decomposition_2x2_eigenvectors));
end;
function transpose(matrix: RealArrayArray): RealArrayArray;
var
  transpose_rows: integer;
  transpose_cols: integer;
  transpose_trans: array of RealArray;
  transpose_i: integer;
  transpose_row: array of real;
  transpose_j: integer;
begin
  transpose_rows := Length(matrix);
  transpose_cols := Length(matrix[0]);
  transpose_trans := [];
  transpose_i := 0;
  while transpose_i < transpose_cols do begin
  transpose_row := [];
  transpose_j := 0;
  while transpose_j < transpose_rows do begin
  transpose_row := concat(transpose_row, [matrix[transpose_j][transpose_i]]);
  transpose_j := transpose_j + 1;
end;
  transpose_trans := concat(transpose_trans, [transpose_row]);
  transpose_i := transpose_i + 1;
end;
  exit(transpose_trans);
end;
function matrix_multiply(a: RealArrayArray; b: RealArrayArray): RealArrayArray;
var
  matrix_multiply_rows_a: integer;
  matrix_multiply_cols_a: integer;
  matrix_multiply_rows_b: integer;
  matrix_multiply_cols_b: integer;
  matrix_multiply_result_: array of RealArray;
  matrix_multiply_i: integer;
  matrix_multiply_row: array of real;
  matrix_multiply_j: integer;
  matrix_multiply_sum: real;
  matrix_multiply_k: integer;
begin
  matrix_multiply_rows_a := Length(a);
  matrix_multiply_cols_a := Length(a[0]);
  matrix_multiply_rows_b := Length(b);
  matrix_multiply_cols_b := Length(b[0]);
  if matrix_multiply_cols_a <> matrix_multiply_rows_b then begin
  panic('Incompatible matrices');
end;
  matrix_multiply_result_ := [];
  matrix_multiply_i := 0;
  while matrix_multiply_i < matrix_multiply_rows_a do begin
  matrix_multiply_row := [];
  matrix_multiply_j := 0;
  while matrix_multiply_j < matrix_multiply_cols_b do begin
  matrix_multiply_sum := 0;
  matrix_multiply_k := 0;
  while matrix_multiply_k < matrix_multiply_cols_a do begin
  matrix_multiply_sum := matrix_multiply_sum + (a[matrix_multiply_i][matrix_multiply_k] * b[matrix_multiply_k][matrix_multiply_j]);
  matrix_multiply_k := matrix_multiply_k + 1;
end;
  matrix_multiply_row := concat(matrix_multiply_row, [matrix_multiply_sum]);
  matrix_multiply_j := matrix_multiply_j + 1;
end;
  matrix_multiply_result_ := concat(matrix_multiply_result_, [matrix_multiply_row]);
  matrix_multiply_i := matrix_multiply_i + 1;
end;
  exit(matrix_multiply_result_);
end;
function apply_pca(data: RealArrayArray; n_components: integer): PCAResult;
var
  apply_pca_standardized: RealArrayArray;
  apply_pca_cov: RealArrayArray;
  apply_pca_eig: Eigen;
  apply_pca_eigenvalues: array of real;
  apply_pca_eigenvectors: array of RealArray;
  apply_pca_components: RealArrayArray;
  apply_pca_transformed: RealArrayArray;
  apply_pca_total: real;
  apply_pca_ratios: array of real;
  apply_pca_i: integer;
begin
  apply_pca_standardized := standardize(data);
  apply_pca_cov := covariance_matrix(apply_pca_standardized);
  apply_pca_eig := eigen_decomposition_2x2(apply_pca_cov);
  apply_pca_eigenvalues := apply_pca_eig.values;
  apply_pca_eigenvectors := apply_pca_eig.vectors;
  apply_pca_components := transpose(apply_pca_eigenvectors);
  apply_pca_transformed := matrix_multiply(apply_pca_standardized, apply_pca_components);
  apply_pca_total := apply_pca_eigenvalues[0] + apply_pca_eigenvalues[1];
  apply_pca_ratios := [];
  apply_pca_i := 0;
  while apply_pca_i < n_components do begin
  apply_pca_ratios := concat(apply_pca_ratios, [apply_pca_eigenvalues[apply_pca_i] / apply_pca_total]);
  apply_pca_i := apply_pca_i + 1;
end;
  exit(makePCAResult(apply_pca_transformed, apply_pca_ratios));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  data := [[2.5, 2.4], [0.5, 0.7], [2.2, 2.9], [1.9, 2.2], [3.1, 3], [2.3, 2.7], [2, 1.6], [1, 1.1], [1.5, 1.6], [1.1, 0.9]];
  result_ := apply_pca(data, 2);
  writeln('Transformed Data (first 5 rows):');
  idx := 0;
  while idx < 5 do begin
  show_list_real(result_.transformed[idx]);
  idx := idx + 1;
end;
  writeln('Explained Variance Ratio:');
  show_list_real(result_.variance_ratio);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
