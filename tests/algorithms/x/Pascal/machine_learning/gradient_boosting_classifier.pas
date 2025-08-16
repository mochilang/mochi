{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type Stump = record
  feature: integer;
  threshold: real;
  left: real;
  right: real;
end;
type RealArray = array of real;
type StumpArray = array of Stump;
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
  features: array of RealArray;
  target: array of real;
  models: StumpArray;
  predictions: RealArray;
  acc: real;
  n_estimators: integer;
  learning_rate: real;
  x: real;
  residuals: RealArray;
  preds: RealArray;
function makeStump(feature: integer; threshold: real; left: real; right: real): Stump; forward;
function exp_approx(x: real): real; forward;
function signf(x: real): real; forward;
function gradient(target: RealArray; preds: RealArray): RealArray; forward;
function predict_raw(models: StumpArray; features: RealArrayArray; learning_rate: real): RealArray; forward;
function predict(models: StumpArray; features: RealArrayArray; learning_rate: real): RealArray; forward;
function train_stump(features: RealArrayArray; residuals: RealArray): Stump; forward;
function fit(n_estimators: integer; learning_rate: real; features: RealArrayArray; target: RealArray): StumpArray; forward;
function accuracy(preds: RealArray; target: RealArray): real; forward;
function makeStump(feature: integer; threshold: real; left: real; right: real): Stump;
begin
  Result.feature := feature;
  Result.threshold := threshold;
  Result.left := left;
  Result.right := right;
end;
function exp_approx(x: real): real;
var
  exp_approx_term: real;
  exp_approx_sum: real;
  exp_approx_i: integer;
begin
  exp_approx_term := 1;
  exp_approx_sum := 1;
  exp_approx_i := 1;
  while exp_approx_i < 10 do begin
  exp_approx_term := (exp_approx_term * x) / Double(exp_approx_i);
  exp_approx_sum := exp_approx_sum + exp_approx_term;
  exp_approx_i := exp_approx_i + 1;
end;
  exit(exp_approx_sum);
end;
function signf(x: real): real;
begin
  if x >= 0 then begin
  exit(1);
end;
  exit(-1);
end;
function gradient(target: RealArray; preds: RealArray): RealArray;
var
  gradient_n: integer;
  gradient_residuals: array of real;
  gradient_i: integer;
  gradient_t: real;
  gradient_y: real;
  gradient_exp_val: real;
  gradient_res: real;
begin
  gradient_n := Length(target);
  gradient_residuals := [];
  gradient_i := 0;
  while gradient_i < gradient_n do begin
  gradient_t := target[gradient_i];
  gradient_y := preds[gradient_i];
  gradient_exp_val := exp_approx(gradient_t * gradient_y);
  gradient_res := -gradient_t / (1 + gradient_exp_val);
  gradient_residuals := concat(gradient_residuals, [gradient_res]);
  gradient_i := gradient_i + 1;
end;
  exit(gradient_residuals);
end;
function predict_raw(models: StumpArray; features: RealArrayArray; learning_rate: real): RealArray;
var
  predict_raw_n: integer;
  predict_raw_preds: array of real;
  predict_raw_i: integer;
  predict_raw_m: integer;
  predict_raw_stump_var: Stump;
  predict_raw_value: real;
begin
  predict_raw_n := Length(features);
  predict_raw_preds := [];
  predict_raw_i := 0;
  while predict_raw_i < predict_raw_n do begin
  predict_raw_preds := concat(predict_raw_preds, [0]);
  predict_raw_i := predict_raw_i + 1;
end;
  predict_raw_m := 0;
  while predict_raw_m < Length(models) do begin
  predict_raw_stump_var := models[predict_raw_m];
  predict_raw_i := 0;
  while predict_raw_i < predict_raw_n do begin
  predict_raw_value := features[predict_raw_i][predict_raw_stump_var.feature];
  if predict_raw_value <= predict_raw_stump_var.threshold then begin
  predict_raw_preds[predict_raw_i] := predict_raw_preds[predict_raw_i] + (learning_rate * predict_raw_stump_var.left);
end else begin
  predict_raw_preds[predict_raw_i] := predict_raw_preds[predict_raw_i] + (learning_rate * predict_raw_stump_var.right);
end;
  predict_raw_i := predict_raw_i + 1;
end;
  predict_raw_m := predict_raw_m + 1;
end;
  exit(predict_raw_preds);
end;
function predict(models: StumpArray; features: RealArrayArray; learning_rate: real): RealArray;
var
  predict_raw_var: RealArray;
  predict_result_: array of real;
  predict_i: integer;
begin
  predict_raw_var := predict_raw(models, features, learning_rate);
  predict_result_ := [];
  predict_i := 0;
  while predict_i < Length(predict_raw_var) do begin
  predict_result_ := concat(predict_result_, [signf(predict_raw_var[predict_i])]);
  predict_i := predict_i + 1;
end;
  exit(predict_result_);
end;
function train_stump(features: RealArrayArray; residuals: RealArray): Stump;
var
  train_stump_n_samples: integer;
  train_stump_n_features: integer;
  train_stump_best_feature: integer;
  train_stump_best_threshold: real;
  train_stump_best_error: real;
  train_stump_best_left: real;
  train_stump_best_right: real;
  train_stump_j: integer;
  train_stump_t_index: integer;
  train_stump_t: real;
  train_stump_sum_left: real;
  train_stump_count_left: integer;
  train_stump_sum_right: real;
  train_stump_count_right: integer;
  train_stump_i: integer;
  train_stump_left_val: real;
  train_stump_right_val: real;
  train_stump_error: real;
  train_stump_pred: real;
  train_stump_diff: real;
begin
  train_stump_n_samples := Length(features);
  train_stump_n_features := Length(features[0]);
  train_stump_best_feature := 0;
  train_stump_best_threshold := 0;
  train_stump_best_error := 1e+09;
  train_stump_best_left := 0;
  train_stump_best_right := 0;
  train_stump_j := 0;
  while train_stump_j < train_stump_n_features do begin
  train_stump_t_index := 0;
  while train_stump_t_index < train_stump_n_samples do begin
  train_stump_t := features[train_stump_t_index][train_stump_j];
  train_stump_sum_left := 0;
  train_stump_count_left := 0;
  train_stump_sum_right := 0;
  train_stump_count_right := 0;
  train_stump_i := 0;
  while train_stump_i < train_stump_n_samples do begin
  if features[train_stump_i][train_stump_j] <= train_stump_t then begin
  train_stump_sum_left := train_stump_sum_left + residuals[train_stump_i];
  train_stump_count_left := train_stump_count_left + 1;
end else begin
  train_stump_sum_right := train_stump_sum_right + residuals[train_stump_i];
  train_stump_count_right := train_stump_count_right + 1;
end;
  train_stump_i := train_stump_i + 1;
end;
  train_stump_left_val := 0;
  if train_stump_count_left <> 0 then begin
  train_stump_left_val := train_stump_sum_left / Double(train_stump_count_left);
end;
  train_stump_right_val := 0;
  if train_stump_count_right <> 0 then begin
  train_stump_right_val := train_stump_sum_right / Double(train_stump_count_right);
end;
  train_stump_error := 0;
  train_stump_i := 0;
  while train_stump_i < train_stump_n_samples do begin
  if features[train_stump_i][train_stump_j] <= train_stump_t then begin
  train_stump_pred := train_stump_left_val;
end else begin
  train_stump_pred := train_stump_right_val;
end;
  train_stump_diff := residuals[train_stump_i] - train_stump_pred;
  train_stump_error := train_stump_error + (train_stump_diff * train_stump_diff);
  train_stump_i := train_stump_i + 1;
end;
  if train_stump_error < train_stump_best_error then begin
  train_stump_best_error := train_stump_error;
  train_stump_best_feature := train_stump_j;
  train_stump_best_threshold := train_stump_t;
  train_stump_best_left := train_stump_left_val;
  train_stump_best_right := train_stump_right_val;
end;
  train_stump_t_index := train_stump_t_index + 1;
end;
  train_stump_j := train_stump_j + 1;
end;
  exit(makeStump(train_stump_best_feature, train_stump_best_threshold, train_stump_best_left, train_stump_best_right));
end;
function fit(n_estimators: integer; learning_rate: real; features: RealArrayArray; target: RealArray): StumpArray;
var
  fit_models: array of Stump;
  fit_m: integer;
  fit_preds: RealArray;
  fit_grad: RealArray;
  fit_residuals: array of real;
  fit_i: integer;
  fit_stump_var: Stump;
begin
  fit_models := [];
  fit_m := 0;
  while fit_m < n_estimators do begin
  fit_preds := predict_raw(fit_models, features, learning_rate);
  fit_grad := gradient(target, fit_preds);
  fit_residuals := [];
  fit_i := 0;
  while fit_i < Length(fit_grad) do begin
  fit_residuals := concat(fit_residuals, [-fit_grad[fit_i]]);
  fit_i := fit_i + 1;
end;
  fit_stump_var := train_stump(features, fit_residuals);
  fit_models := concat(fit_models, [fit_stump_var]);
  fit_m := fit_m + 1;
end;
  exit(fit_models);
end;
function accuracy(preds: RealArray; target: RealArray): real;
var
  accuracy_n: integer;
  accuracy_correct: integer;
  accuracy_i: integer;
begin
  accuracy_n := Length(target);
  accuracy_correct := 0;
  accuracy_i := 0;
  while accuracy_i < accuracy_n do begin
  if preds[accuracy_i] = target[accuracy_i] then begin
  accuracy_correct := accuracy_correct + 1;
end;
  accuracy_i := accuracy_i + 1;
end;
  exit(Double(accuracy_correct) / Double(accuracy_n));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  features := [[1], [2], [3], [4]];
  target := [-1, -1, 1, 1];
  models := fit(5, 0.5, features, target);
  predictions := predict(models, features, 0.5);
  acc := accuracy(predictions, target);
  writeln('Accuracy: ' + FloatToStr(acc));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
