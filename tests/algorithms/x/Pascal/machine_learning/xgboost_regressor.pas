{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Tree = record
  threshold: real;
  left_value: real;
  right_value: real;
end;
type RealArray = array of real;
type TreeArray = array of Tree;
type RealArrayArray = array of RealArray;
type Dataset = record
  data: array of RealArray;
  target: array of real;
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
  target: RealArray;
  test_features: RealArrayArray;
  y_pred: RealArray;
  features: RealArrayArray;
  dataset_var: Dataset;
  y_true: RealArray;
function makeTree(threshold: real; left_value: real; right_value: real): Tree; forward;
function makeDataset(data: RealArrayArray; target: RealArray): Dataset; forward;
function data_handling(data_handling_dataset_var: Dataset): Dataset; forward;
function xgboost(features: RealArrayArray; target: RealArray; test_features: RealArrayArray): RealArray; forward;
function mean_absolute_error(y_true: RealArray; y_pred: RealArray): real; forward;
function mean_squared_error(y_true: RealArray; y_pred: RealArray): real; forward;
procedure main(); forward;
function makeTree(threshold: real; left_value: real; right_value: real): Tree;
begin
  Result.threshold := threshold;
  Result.left_value := left_value;
  Result.right_value := right_value;
end;
function makeDataset(data: RealArrayArray; target: RealArray): Dataset;
begin
  Result.data := data;
  Result.target := target;
end;
function data_handling(data_handling_dataset_var: Dataset): Dataset;
begin
  exit(dataset_var);
end;
function xgboost(features: RealArrayArray; target: RealArray; test_features: RealArrayArray): RealArray;
var
  xgboost_learning_rate: real;
  xgboost_n_estimators: integer;
  xgboost_trees: array of Tree;
  xgboost_predictions: array of real;
  xgboost_i: integer;
  xgboost_est: integer;
  xgboost_residuals: array of real;
  xgboost_j: integer;
  xgboost_sum_feat: real;
  xgboost_threshold: real;
  xgboost_left_sum: real;
  xgboost_left_count: integer;
  xgboost_right_sum: real;
  xgboost_right_count: integer;
  xgboost_left_value: real;
  xgboost_right_value: real;
  xgboost_preds: array of real;
  xgboost_t: integer;
  xgboost_pred: real;
  xgboost_k: integer;
begin
  xgboost_learning_rate := 0.5;
  xgboost_n_estimators := 3;
  xgboost_trees := [];
  xgboost_predictions := [];
  xgboost_i := 0;
  while xgboost_i < Length(target) do begin
  xgboost_predictions := concat(xgboost_predictions, [0]);
  xgboost_i := xgboost_i + 1;
end;
  xgboost_est := 0;
  while xgboost_est < xgboost_n_estimators do begin
  xgboost_residuals := [];
  xgboost_j := 0;
  while xgboost_j < Length(target) do begin
  xgboost_residuals := concat(xgboost_residuals, [target[xgboost_j] - xgboost_predictions[xgboost_j]]);
  xgboost_j := xgboost_j + 1;
end;
  xgboost_sum_feat := 0;
  xgboost_j := 0;
  while xgboost_j < Length(features) do begin
  xgboost_sum_feat := xgboost_sum_feat + features[xgboost_j][0];
  xgboost_j := xgboost_j + 1;
end;
  xgboost_threshold := xgboost_sum_feat / Double(Length(features));
  xgboost_left_sum := 0;
  xgboost_left_count := 0;
  xgboost_right_sum := 0;
  xgboost_right_count := 0;
  xgboost_j := 0;
  while xgboost_j < Length(features) do begin
  if features[xgboost_j][0] <= xgboost_threshold then begin
  xgboost_left_sum := xgboost_left_sum + xgboost_residuals[xgboost_j];
  xgboost_left_count := xgboost_left_count + 1;
end else begin
  xgboost_right_sum := xgboost_right_sum + xgboost_residuals[xgboost_j];
  xgboost_right_count := xgboost_right_count + 1;
end;
  xgboost_j := xgboost_j + 1;
end;
  xgboost_left_value := 0;
  if xgboost_left_count > 0 then begin
  xgboost_left_value := xgboost_left_sum / Double(xgboost_left_count);
end;
  xgboost_right_value := 0;
  if xgboost_right_count > 0 then begin
  xgboost_right_value := xgboost_right_sum / Double(xgboost_right_count);
end;
  xgboost_j := 0;
  while xgboost_j < Length(features) do begin
  if features[xgboost_j][0] <= xgboost_threshold then begin
  xgboost_predictions[xgboost_j] := xgboost_predictions[xgboost_j] + (xgboost_learning_rate * xgboost_left_value);
end else begin
  xgboost_predictions[xgboost_j] := xgboost_predictions[xgboost_j] + (xgboost_learning_rate * xgboost_right_value);
end;
  xgboost_j := xgboost_j + 1;
end;
  xgboost_trees := concat(xgboost_trees, [makeTree(xgboost_threshold, xgboost_left_value, xgboost_right_value)]);
  xgboost_est := xgboost_est + 1;
end;
  xgboost_preds := [];
  xgboost_t := 0;
  while xgboost_t < Length(test_features) do begin
  xgboost_pred := 0;
  xgboost_k := 0;
  while xgboost_k < Length(xgboost_trees) do begin
  if test_features[xgboost_t][0] <= xgboost_trees[xgboost_k].threshold then begin
  xgboost_pred := xgboost_pred + (xgboost_learning_rate * xgboost_trees[xgboost_k].left_value);
end else begin
  xgboost_pred := xgboost_pred + (xgboost_learning_rate * xgboost_trees[xgboost_k].right_value);
end;
  xgboost_k := xgboost_k + 1;
end;
  xgboost_preds := concat(xgboost_preds, [xgboost_pred]);
  xgboost_t := xgboost_t + 1;
end;
  exit(xgboost_preds);
end;
function mean_absolute_error(y_true: RealArray; y_pred: RealArray): real;
var
  mean_absolute_error_sum: real;
  mean_absolute_error_i: integer;
  mean_absolute_error_diff: real;
begin
  mean_absolute_error_sum := 0;
  mean_absolute_error_i := 0;
  while mean_absolute_error_i < Length(y_true) do begin
  mean_absolute_error_diff := y_true[mean_absolute_error_i] - y_pred[mean_absolute_error_i];
  if mean_absolute_error_diff < 0 then begin
  mean_absolute_error_diff := -mean_absolute_error_diff;
end;
  mean_absolute_error_sum := mean_absolute_error_sum + mean_absolute_error_diff;
  mean_absolute_error_i := mean_absolute_error_i + 1;
end;
  exit(mean_absolute_error_sum / Double(Length(y_true)));
end;
function mean_squared_error(y_true: RealArray; y_pred: RealArray): real;
var
  mean_squared_error_sum: real;
  mean_squared_error_i: integer;
  mean_squared_error_diff: real;
begin
  mean_squared_error_sum := 0;
  mean_squared_error_i := 0;
  while mean_squared_error_i < Length(y_true) do begin
  mean_squared_error_diff := y_true[mean_squared_error_i] - y_pred[mean_squared_error_i];
  mean_squared_error_sum := mean_squared_error_sum + (mean_squared_error_diff * mean_squared_error_diff);
  mean_squared_error_i := mean_squared_error_i + 1;
end;
  exit(mean_squared_error_sum / Double(Length(y_true)));
end;
procedure main();
var
  main_california: Dataset;
  main_ds: Dataset;
  main_x_train: array of RealArray;
  main_y_train: array of real;
  main_x_test: array of array of real;
  main_y_test: array of real;
  main_predictions: RealArray;
begin
  main_california := makeDataset([[1], [2], [3], [4]], [2, 3, 4, 5]);
  main_ds := data_handling(main_california);
  main_x_train := main_ds.data;
  main_y_train := main_ds.target;
  main_x_test := [[1.5], [3.5]];
  main_y_test := [2.5, 4.5];
  main_predictions := xgboost(main_x_train, main_y_train, main_x_test);
  writeln('Predictions:');
  show_list_real(main_predictions);
  writeln('Mean Absolute Error:');
  writeln(mean_absolute_error(main_y_test, main_predictions));
  writeln('Mean Square Error:');
  writeln(mean_squared_error(main_y_test, main_predictions));
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
