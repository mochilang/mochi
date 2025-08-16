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
type IntArray = array of integer;
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
  residuals: RealArray;
  s: Stump;
  xs: RealArray;
  x: RealArray;
  rounds: integer;
  model: StumpArray;
  features: RealArrayArray;
  targets: IntArray;
function makeStump(feature: integer; threshold: real; left: real; right: real): Stump; forward;
function mean(xs: RealArray): real; forward;
function stump_predict(s: Stump; x: RealArray): real; forward;
function train_stump(features: RealArrayArray; residuals: RealArray): Stump; forward;
function boost(features: RealArrayArray; targets: IntArray; rounds: integer): StumpArray; forward;
function predict(model: StumpArray; x: RealArray): real; forward;
procedure main(); forward;
function makeStump(feature: integer; threshold: real; left: real; right: real): Stump;
begin
  Result.feature := feature;
  Result.threshold := threshold;
  Result.left := left;
  Result.right := right;
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
  exit(mean_sum / (Length(xs) * 1));
end;
function stump_predict(s: Stump; x: RealArray): real;
begin
  if x[s.feature] < s.threshold then begin
  exit(s.left);
end;
  exit(s.right);
end;
function train_stump(features: RealArrayArray; residuals: RealArray): Stump;
var
  train_stump_best_feature: integer;
  train_stump_best_threshold: real;
  train_stump_best_error: real;
  train_stump_best_left: real;
  train_stump_best_right: real;
  train_stump_num_features: integer;
  train_stump_f: integer;
  train_stump_i: integer;
  train_stump_threshold: real;
  train_stump_left: array of real;
  train_stump_right: array of real;
  train_stump_j: integer;
  train_stump_left_mean: real;
  train_stump_right_mean: real;
  train_stump_err: real;
  train_stump_pred: real;
  train_stump_diff: real;
begin
  train_stump_best_feature := 0;
  train_stump_best_threshold := 0;
  train_stump_best_error := 1e+09;
  train_stump_best_left := 0;
  train_stump_best_right := 0;
  train_stump_num_features := Length(features[0]);
  train_stump_f := 0;
  while train_stump_f < train_stump_num_features do begin
  train_stump_i := 0;
  while train_stump_i < Length(features) do begin
  train_stump_threshold := features[train_stump_i][train_stump_f];
  train_stump_left := [];
  train_stump_right := [];
  train_stump_j := 0;
  while train_stump_j < Length(features) do begin
  if features[train_stump_j][train_stump_f] < train_stump_threshold then begin
  train_stump_left := concat(train_stump_left, [residuals[train_stump_j]]);
end else begin
  train_stump_right := concat(train_stump_right, [residuals[train_stump_j]]);
end;
  train_stump_j := train_stump_j + 1;
end;
  if (Length(train_stump_left) <> 0) and (Length(train_stump_right) <> 0) then begin
  train_stump_left_mean := mean(train_stump_left);
  train_stump_right_mean := mean(train_stump_right);
  train_stump_err := 0;
  train_stump_j := 0;
  while train_stump_j < Length(features) do begin
  if features[train_stump_j][train_stump_f] < train_stump_threshold then begin
  train_stump_pred := train_stump_left_mean;
end else begin
  train_stump_pred := train_stump_right_mean;
end;
  train_stump_diff := residuals[train_stump_j] - train_stump_pred;
  train_stump_err := train_stump_err + (train_stump_diff * train_stump_diff);
  train_stump_j := train_stump_j + 1;
end;
  if train_stump_err < train_stump_best_error then begin
  train_stump_best_error := train_stump_err;
  train_stump_best_feature := train_stump_f;
  train_stump_best_threshold := train_stump_threshold;
  train_stump_best_left := train_stump_left_mean;
  train_stump_best_right := train_stump_right_mean;
end;
end;
  train_stump_i := train_stump_i + 1;
end;
  train_stump_f := train_stump_f + 1;
end;
  exit(makeStump(train_stump_best_feature, train_stump_best_threshold, train_stump_best_left, train_stump_best_right));
end;
function boost(features: RealArrayArray; targets: IntArray; rounds: integer): StumpArray;
var
  boost_model: array of Stump;
  boost_preds: array of real;
  boost_i: integer;
  boost_r: integer;
  boost_residuals: array of real;
  boost_j: integer;
  boost_stump_var: Stump;
begin
  boost_model := [];
  boost_preds := [];
  boost_i := 0;
  while boost_i < Length(targets) do begin
  boost_preds := concat(boost_preds, [0]);
  boost_i := boost_i + 1;
end;
  boost_r := 0;
  while boost_r < rounds do begin
  boost_residuals := [];
  boost_j := 0;
  while boost_j < Length(targets) do begin
  boost_residuals := concat(boost_residuals, [targets[boost_j] - boost_preds[boost_j]]);
  boost_j := boost_j + 1;
end;
  boost_stump_var := train_stump(features, boost_residuals);
  boost_model := concat(boost_model, [boost_stump_var]);
  boost_j := 0;
  while boost_j < Length(boost_preds) do begin
  boost_preds[boost_j] := boost_preds[boost_j] + stump_predict(boost_stump_var, features[boost_j]);
  boost_j := boost_j + 1;
end;
  boost_r := boost_r + 1;
end;
  exit(boost_model);
end;
function predict(model: StumpArray; x: RealArray): real;
var
  predict_score: real;
  predict_i: integer;
  predict_s: Stump;
begin
  predict_score := 0;
  predict_i := 0;
  while predict_i < Length(model) do begin
  predict_s := model[predict_i];
  if x[predict_s.feature] < predict_s.threshold then begin
  predict_score := predict_score + predict_s.left;
end else begin
  predict_score := predict_score + predict_s.right;
end;
  predict_i := predict_i + 1;
end;
  exit(predict_score);
end;
procedure main();
var
  main_features: array of RealArray;
  main_targets: array of integer;
  main_model: array of Stump;
  main_out_: string;
  main_i: integer;
  main_s: real;
  main_label_: integer;
begin
  main_features := [[5.1, 3.5], [4.9, 3], [6.2, 3.4], [5.9, 3]];
  main_targets := [0, 0, 1, 1];
  main_model := boost(main_features, main_targets, 3);
  main_out_ := '';
  main_i := 0;
  while main_i < Length(main_features) do begin
  main_s := predict(main_model, main_features[main_i]);
  if main_s >= 0.5 then begin
  main_label_ := 1;
end else begin
  main_label_ := 0;
end;
  if main_i = 0 then begin
  main_out_ := IntToStr(main_label_);
end else begin
  main_out_ := (main_out_ + ' ') + IntToStr(main_label_);
end;
  main_i := main_i + 1;
end;
  writeln(main_out_);
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
