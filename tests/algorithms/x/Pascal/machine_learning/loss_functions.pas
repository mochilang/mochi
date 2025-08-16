{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
type RealArrayArray = array of RealArray;
type RealArrayArrayArray = array of RealArrayArray;
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
  beta: real;
  v: RealArray;
  exp__var: real;
  epsilon: real;
  gamma: real;
  y_true: RealArray;
  y_pred: RealArray;
  alpha: RealArray;
  x: real;
  delta: real;
  base: real;
  a: real;
  b: real;
  hi: real;
  lo: real;
function absf(x: real): real; forward;
function maxf(a: real; b: real): real; forward;
function minf(a: real; b: real): real; forward;
function clip(x: real; lo: real; hi: real): real; forward;
function to_float(x: integer): real; forward;
function powf(base: real; powf_exp__var: real): real; forward;
function ln_(x: real): real; forward;
function exp_(x: real): real; forward;
function mean(v: RealArray): real; forward;
function binary_cross_entropy(y_true: RealArray; y_pred: RealArray; epsilon: real): real; forward;
function binary_focal_cross_entropy(y_true: RealArray; y_pred: RealArray; gamma: real; alpha: real; epsilon: real): real; forward;
function categorical_cross_entropy(y_true: RealArrayArray; y_pred: RealArrayArray; epsilon: real): real; forward;
function categorical_focal_cross_entropy(y_true: RealArrayArray; y_pred: RealArrayArray; alpha: RealArray; gamma: real; epsilon: real): real; forward;
function hinge_loss(y_true: RealArray; y_pred: RealArray): real; forward;
function huber_loss(y_true: RealArray; y_pred: RealArray; delta: real): real; forward;
function mean_squared_error(y_true: RealArray; y_pred: RealArray): real; forward;
function mean_absolute_error(y_true: RealArray; y_pred: RealArray): real; forward;
function mean_squared_logarithmic_error(y_true: RealArray; y_pred: RealArray): real; forward;
function mean_absolute_percentage_error(y_true: RealArray; y_pred: RealArray; epsilon: real): real; forward;
function perplexity_loss(y_true: IntArrayArray; y_pred: RealArrayArrayArray; epsilon: real): real; forward;
function smooth_l1_loss(y_true: RealArray; y_pred: RealArray; beta: real): real; forward;
function kullback_leibler_divergence(y_true: RealArray; y_pred: RealArray): real; forward;
procedure main(); forward;
function absf(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function maxf(a: real; b: real): real;
begin
  if a > b then begin
  exit(a);
end;
  exit(b);
end;
function minf(a: real; b: real): real;
begin
  if a < b then begin
  exit(a);
end;
  exit(b);
end;
function clip(x: real; lo: real; hi: real): real;
begin
  exit(maxf(lo, minf(x, hi)));
end;
function to_float(x: integer): real;
begin
  exit(x * 1);
end;
function powf(base: real; powf_exp__var: real): real;
var
  powf_result_: real;
  powf_i: integer;
  powf_n: integer;
begin
  powf_result_ := 1;
  powf_i := 0;
  powf_n := Trunc(exp__var);
  while powf_i < powf_n do begin
  powf_result_ := powf_result_ * base;
  powf_i := powf_i + 1;
end;
  exit(powf_result_);
end;
function ln_(x: real): real;
var
  ln__y: real;
  ln__y2: real;
  ln__term: real;
  ln__sum: real;
  ln__k: integer;
  ln__denom: real;
begin
  if x <= 0 then begin
  panic('ln domain error');
end;
  ln__y := (x - 1) / (x + 1);
  ln__y2 := ln__y * ln__y;
  ln__term := ln__y;
  ln__sum := 0;
  ln__k := 0;
  while ln__k < 10 do begin
  ln__denom := to_float((2 * ln__k) + 1);
  ln__sum := ln__sum + (ln__term / ln__denom);
  ln__term := ln__term * ln__y2;
  ln__k := ln__k + 1;
end;
  exit(2 * ln__sum);
end;
function exp_(x: real): real;
var
  exp__term: real;
  exp__sum: real;
  exp__n: integer;
begin
  exp__term := 1;
  exp__sum := 1;
  exp__n := 1;
  while exp__n < 20 do begin
  exp__term := (exp__term * x) / to_float(exp__n);
  exp__sum := exp__sum + exp__term;
  exp__n := exp__n + 1;
end;
  exit(exp__sum);
end;
function mean(v: RealArray): real;
var
  mean_total: real;
  mean_i: integer;
begin
  mean_total := 0;
  mean_i := 0;
  while mean_i < Length(v) do begin
  mean_total := mean_total + v[mean_i];
  mean_i := mean_i + 1;
end;
  exit(mean_total / to_float(Length(v)));
end;
function binary_cross_entropy(y_true: RealArray; y_pred: RealArray; epsilon: real): real;
var
  binary_cross_entropy_losses: array of real;
  binary_cross_entropy_i: integer;
  binary_cross_entropy_yt: real;
  binary_cross_entropy_yp: real;
  binary_cross_entropy_loss: real;
begin
  if Length(y_true) <> Length(y_pred) then begin
  panic('Input arrays must have the same length.');
end;
  binary_cross_entropy_losses := [];
  binary_cross_entropy_i := 0;
  while binary_cross_entropy_i < Length(y_true) do begin
  binary_cross_entropy_yt := y_true[binary_cross_entropy_i];
  binary_cross_entropy_yp := clip(y_pred[binary_cross_entropy_i], epsilon, 1 - epsilon);
  binary_cross_entropy_loss := -((binary_cross_entropy_yt * ln(binary_cross_entropy_yp)) + ((1 - binary_cross_entropy_yt) * ln(1 - binary_cross_entropy_yp)));
  binary_cross_entropy_losses := concat(binary_cross_entropy_losses, [binary_cross_entropy_loss]);
  binary_cross_entropy_i := binary_cross_entropy_i + 1;
end;
  exit(mean(binary_cross_entropy_losses));
end;
function binary_focal_cross_entropy(y_true: RealArray; y_pred: RealArray; gamma: real; alpha: real; epsilon: real): real;
var
  binary_focal_cross_entropy_losses: array of real;
  binary_focal_cross_entropy_i: integer;
  binary_focal_cross_entropy_yt: real;
  binary_focal_cross_entropy_yp: real;
  binary_focal_cross_entropy_term1: real;
  binary_focal_cross_entropy_term2: real;
begin
  if Length(y_true) <> Length(y_pred) then begin
  panic('Input arrays must have the same length.');
end;
  binary_focal_cross_entropy_losses := [];
  binary_focal_cross_entropy_i := 0;
  while binary_focal_cross_entropy_i < Length(y_true) do begin
  binary_focal_cross_entropy_yt := y_true[binary_focal_cross_entropy_i];
  binary_focal_cross_entropy_yp := clip(y_pred[binary_focal_cross_entropy_i], epsilon, 1 - epsilon);
  binary_focal_cross_entropy_term1 := ((alpha * powf(1 - binary_focal_cross_entropy_yp, gamma)) * binary_focal_cross_entropy_yt) * ln(binary_focal_cross_entropy_yp);
  binary_focal_cross_entropy_term2 := (((1 - alpha) * powf(binary_focal_cross_entropy_yp, gamma)) * (1 - binary_focal_cross_entropy_yt)) * ln(1 - binary_focal_cross_entropy_yp);
  binary_focal_cross_entropy_losses := concat(binary_focal_cross_entropy_losses, [-(binary_focal_cross_entropy_term1 + binary_focal_cross_entropy_term2)]);
  binary_focal_cross_entropy_i := binary_focal_cross_entropy_i + 1;
end;
  exit(mean(binary_focal_cross_entropy_losses));
end;
function categorical_cross_entropy(y_true: RealArrayArray; y_pred: RealArrayArray; epsilon: real): real;
var
  categorical_cross_entropy_rows: integer;
  categorical_cross_entropy_total: real;
  categorical_cross_entropy_i: integer;
  categorical_cross_entropy_sum_true: real;
  categorical_cross_entropy_sum_pred: real;
  categorical_cross_entropy_j: integer;
  categorical_cross_entropy_yt: real;
  categorical_cross_entropy_yp: real;
begin
  if Length(y_true) <> Length(y_pred) then begin
  panic('Input arrays must have the same shape.');
end;
  categorical_cross_entropy_rows := Length(y_true);
  categorical_cross_entropy_total := 0;
  categorical_cross_entropy_i := 0;
  while categorical_cross_entropy_i < categorical_cross_entropy_rows do begin
  if Length(y_true[categorical_cross_entropy_i]) <> Length(y_pred[categorical_cross_entropy_i]) then begin
  panic('Input arrays must have the same shape.');
end;
  categorical_cross_entropy_sum_true := 0;
  categorical_cross_entropy_sum_pred := 0;
  categorical_cross_entropy_j := 0;
  while categorical_cross_entropy_j < Length(y_true[categorical_cross_entropy_i]) do begin
  categorical_cross_entropy_yt := y_true[categorical_cross_entropy_i][categorical_cross_entropy_j];
  categorical_cross_entropy_yp := y_pred[categorical_cross_entropy_i][categorical_cross_entropy_j];
  if (categorical_cross_entropy_yt <> 0) and (categorical_cross_entropy_yt <> 1) then begin
  panic('y_true must be one-hot encoded.');
end;
  categorical_cross_entropy_sum_true := categorical_cross_entropy_sum_true + categorical_cross_entropy_yt;
  categorical_cross_entropy_sum_pred := categorical_cross_entropy_sum_pred + categorical_cross_entropy_yp;
  categorical_cross_entropy_j := categorical_cross_entropy_j + 1;
end;
  if categorical_cross_entropy_sum_true <> 1 then begin
  panic('y_true must be one-hot encoded.');
end;
  if absf(categorical_cross_entropy_sum_pred - 1) > epsilon then begin
  panic('Predicted probabilities must sum to approximately 1.');
end;
  categorical_cross_entropy_j := 0;
  while categorical_cross_entropy_j < Length(y_true[categorical_cross_entropy_i]) do begin
  categorical_cross_entropy_yp := clip(y_pred[categorical_cross_entropy_i][categorical_cross_entropy_j], epsilon, 1);
  categorical_cross_entropy_total := categorical_cross_entropy_total - (y_true[categorical_cross_entropy_i][categorical_cross_entropy_j] * ln(categorical_cross_entropy_yp));
  categorical_cross_entropy_j := categorical_cross_entropy_j + 1;
end;
  categorical_cross_entropy_i := categorical_cross_entropy_i + 1;
end;
  exit(categorical_cross_entropy_total);
end;
function categorical_focal_cross_entropy(y_true: RealArrayArray; y_pred: RealArrayArray; alpha: RealArray; gamma: real; epsilon: real): real;
var
  categorical_focal_cross_entropy_rows: integer;
  categorical_focal_cross_entropy_cols: integer;
  categorical_focal_cross_entropy_a: array of real;
  categorical_focal_cross_entropy_tmp: array of real;
  categorical_focal_cross_entropy_j: integer;
  categorical_focal_cross_entropy_total: real;
  categorical_focal_cross_entropy_i: integer;
  categorical_focal_cross_entropy_sum_true: real;
  categorical_focal_cross_entropy_sum_pred: real;
  categorical_focal_cross_entropy_j_46: integer;
  categorical_focal_cross_entropy_yt: real;
  categorical_focal_cross_entropy_yp: real;
  categorical_focal_cross_entropy_row_loss: real;
begin
  if Length(y_true) <> Length(y_pred) then begin
  panic('Shape of y_true and y_pred must be the same.');
end;
  categorical_focal_cross_entropy_rows := Length(y_true);
  categorical_focal_cross_entropy_cols := Length(y_true[0]);
  categorical_focal_cross_entropy_a := alpha;
  if Length(categorical_focal_cross_entropy_a) = 0 then begin
  categorical_focal_cross_entropy_tmp := [];
  categorical_focal_cross_entropy_j := 0;
  while categorical_focal_cross_entropy_j < categorical_focal_cross_entropy_cols do begin
  categorical_focal_cross_entropy_tmp := concat(categorical_focal_cross_entropy_tmp, [1]);
  categorical_focal_cross_entropy_j := categorical_focal_cross_entropy_j + 1;
end;
  categorical_focal_cross_entropy_a := categorical_focal_cross_entropy_tmp;
end;
  if Length(categorical_focal_cross_entropy_a) <> categorical_focal_cross_entropy_cols then begin
  panic('Length of alpha must match the number of classes.');
end;
  categorical_focal_cross_entropy_total := 0;
  categorical_focal_cross_entropy_i := 0;
  while categorical_focal_cross_entropy_i < categorical_focal_cross_entropy_rows do begin
  if (Length(y_true[categorical_focal_cross_entropy_i]) <> categorical_focal_cross_entropy_cols) or (Length(y_pred[categorical_focal_cross_entropy_i]) <> categorical_focal_cross_entropy_cols) then begin
  panic('Shape of y_true and y_pred must be the same.');
end;
  categorical_focal_cross_entropy_sum_true := 0;
  categorical_focal_cross_entropy_sum_pred := 0;
  categorical_focal_cross_entropy_j_46 := 0;
  while categorical_focal_cross_entropy_j_46 < categorical_focal_cross_entropy_cols do begin
  categorical_focal_cross_entropy_yt := y_true[categorical_focal_cross_entropy_i][categorical_focal_cross_entropy_j_46];
  categorical_focal_cross_entropy_yp := y_pred[categorical_focal_cross_entropy_i][categorical_focal_cross_entropy_j_46];
  if (categorical_focal_cross_entropy_yt <> 0) and (categorical_focal_cross_entropy_yt <> 1) then begin
  panic('y_true must be one-hot encoded.');
end;
  categorical_focal_cross_entropy_sum_true := categorical_focal_cross_entropy_sum_true + categorical_focal_cross_entropy_yt;
  categorical_focal_cross_entropy_sum_pred := categorical_focal_cross_entropy_sum_pred + categorical_focal_cross_entropy_yp;
  categorical_focal_cross_entropy_j_46 := categorical_focal_cross_entropy_j_46 + 1;
end;
  if categorical_focal_cross_entropy_sum_true <> 1 then begin
  panic('y_true must be one-hot encoded.');
end;
  if absf(categorical_focal_cross_entropy_sum_pred - 1) > epsilon then begin
  panic('Predicted probabilities must sum to approximately 1.');
end;
  categorical_focal_cross_entropy_row_loss := 0;
  categorical_focal_cross_entropy_j_46 := 0;
  while categorical_focal_cross_entropy_j_46 < categorical_focal_cross_entropy_cols do begin
  categorical_focal_cross_entropy_yp := clip(y_pred[categorical_focal_cross_entropy_i][categorical_focal_cross_entropy_j_46], epsilon, 1);
  categorical_focal_cross_entropy_row_loss := categorical_focal_cross_entropy_row_loss + (((categorical_focal_cross_entropy_a[categorical_focal_cross_entropy_j_46] * powf(1 - categorical_focal_cross_entropy_yp, gamma)) * y_true[categorical_focal_cross_entropy_i][categorical_focal_cross_entropy_j_46]) * ln(categorical_focal_cross_entropy_yp));
  categorical_focal_cross_entropy_j_46 := categorical_focal_cross_entropy_j_46 + 1;
end;
  categorical_focal_cross_entropy_total := categorical_focal_cross_entropy_total - categorical_focal_cross_entropy_row_loss;
  categorical_focal_cross_entropy_i := categorical_focal_cross_entropy_i + 1;
end;
  exit(categorical_focal_cross_entropy_total / to_float(categorical_focal_cross_entropy_rows));
end;
function hinge_loss(y_true: RealArray; y_pred: RealArray): real;
var
  hinge_loss_losses: array of real;
  hinge_loss_i: integer;
  hinge_loss_yt: real;
  hinge_loss_pred: real;
  hinge_loss_l: real;
begin
  if Length(y_true) <> Length(y_pred) then begin
  panic('Length of predicted and actual array must be same.');
end;
  hinge_loss_losses := [];
  hinge_loss_i := 0;
  while hinge_loss_i < Length(y_true) do begin
  hinge_loss_yt := y_true[hinge_loss_i];
  if (hinge_loss_yt <> -1) and (hinge_loss_yt <> 1) then begin
  panic('y_true can have values -1 or 1 only.');
end;
  hinge_loss_pred := y_pred[hinge_loss_i];
  hinge_loss_l := maxf(0, 1 - (hinge_loss_yt * hinge_loss_pred));
  hinge_loss_losses := concat(hinge_loss_losses, [hinge_loss_l]);
  hinge_loss_i := hinge_loss_i + 1;
end;
  exit(mean(hinge_loss_losses));
end;
function huber_loss(y_true: RealArray; y_pred: RealArray; delta: real): real;
var
  huber_loss_total: real;
  huber_loss_i: integer;
  huber_loss_diff: real;
  huber_loss_adiff: real;
begin
  if Length(y_true) <> Length(y_pred) then begin
  panic('Input arrays must have the same length.');
end;
  huber_loss_total := 0;
  huber_loss_i := 0;
  while huber_loss_i < Length(y_true) do begin
  huber_loss_diff := y_true[huber_loss_i] - y_pred[huber_loss_i];
  huber_loss_adiff := absf(huber_loss_diff);
  if huber_loss_adiff <= delta then begin
  huber_loss_total := huber_loss_total + ((0.5 * huber_loss_diff) * huber_loss_diff);
end else begin
  huber_loss_total := huber_loss_total + (delta * (huber_loss_adiff - (0.5 * delta)));
end;
  huber_loss_i := huber_loss_i + 1;
end;
  exit(huber_loss_total / to_float(Length(y_true)));
end;
function mean_squared_error(y_true: RealArray; y_pred: RealArray): real;
var
  mean_squared_error_losses: array of real;
  mean_squared_error_i: integer;
  mean_squared_error_diff: real;
begin
  if Length(y_true) <> Length(y_pred) then begin
  panic('Input arrays must have the same length.');
end;
  mean_squared_error_losses := [];
  mean_squared_error_i := 0;
  while mean_squared_error_i < Length(y_true) do begin
  mean_squared_error_diff := y_true[mean_squared_error_i] - y_pred[mean_squared_error_i];
  mean_squared_error_losses := concat(mean_squared_error_losses, [mean_squared_error_diff * mean_squared_error_diff]);
  mean_squared_error_i := mean_squared_error_i + 1;
end;
  exit(mean(mean_squared_error_losses));
end;
function mean_absolute_error(y_true: RealArray; y_pred: RealArray): real;
var
  mean_absolute_error_total: real;
  mean_absolute_error_i: integer;
begin
  if Length(y_true) <> Length(y_pred) then begin
  panic('Input arrays must have the same length.');
end;
  mean_absolute_error_total := 0;
  mean_absolute_error_i := 0;
  while mean_absolute_error_i < Length(y_true) do begin
  mean_absolute_error_total := mean_absolute_error_total + absf(y_true[mean_absolute_error_i] - y_pred[mean_absolute_error_i]);
  mean_absolute_error_i := mean_absolute_error_i + 1;
end;
  exit(mean_absolute_error_total / to_float(Length(y_true)));
end;
function mean_squared_logarithmic_error(y_true: RealArray; y_pred: RealArray): real;
var
  mean_squared_logarithmic_error_total: real;
  mean_squared_logarithmic_error_i: integer;
  mean_squared_logarithmic_error_a: real;
  mean_squared_logarithmic_error_b: real;
  mean_squared_logarithmic_error_diff: real;
begin
  if Length(y_true) <> Length(y_pred) then begin
  panic('Input arrays must have the same length.');
end;
  mean_squared_logarithmic_error_total := 0;
  mean_squared_logarithmic_error_i := 0;
  while mean_squared_logarithmic_error_i < Length(y_true) do begin
  mean_squared_logarithmic_error_a := ln(1 + y_true[mean_squared_logarithmic_error_i]);
  mean_squared_logarithmic_error_b := ln(1 + y_pred[mean_squared_logarithmic_error_i]);
  mean_squared_logarithmic_error_diff := mean_squared_logarithmic_error_a - mean_squared_logarithmic_error_b;
  mean_squared_logarithmic_error_total := mean_squared_logarithmic_error_total + (mean_squared_logarithmic_error_diff * mean_squared_logarithmic_error_diff);
  mean_squared_logarithmic_error_i := mean_squared_logarithmic_error_i + 1;
end;
  exit(mean_squared_logarithmic_error_total / to_float(Length(y_true)));
end;
function mean_absolute_percentage_error(y_true: RealArray; y_pred: RealArray; epsilon: real): real;
var
  mean_absolute_percentage_error_total: real;
  mean_absolute_percentage_error_i: integer;
  mean_absolute_percentage_error_yt: real;
begin
  if Length(y_true) <> Length(y_pred) then begin
  panic('The length of the two arrays should be the same.');
end;
  mean_absolute_percentage_error_total := 0;
  mean_absolute_percentage_error_i := 0;
  while mean_absolute_percentage_error_i < Length(y_true) do begin
  mean_absolute_percentage_error_yt := y_true[mean_absolute_percentage_error_i];
  if mean_absolute_percentage_error_yt = 0 then begin
  mean_absolute_percentage_error_yt := epsilon;
end;
  mean_absolute_percentage_error_total := mean_absolute_percentage_error_total + absf((mean_absolute_percentage_error_yt - y_pred[mean_absolute_percentage_error_i]) / mean_absolute_percentage_error_yt);
  mean_absolute_percentage_error_i := mean_absolute_percentage_error_i + 1;
end;
  exit(mean_absolute_percentage_error_total / to_float(Length(y_true)));
end;
function perplexity_loss(y_true: IntArrayArray; y_pred: RealArrayArrayArray; epsilon: real): real;
var
  perplexity_loss_batch: integer;
  perplexity_loss_sentence_len: integer;
  perplexity_loss_vocab_size: integer;
  perplexity_loss_b: integer;
  perplexity_loss_total_perp: real;
  perplexity_loss_sum_log: real;
  perplexity_loss_j: integer;
  perplexity_loss_label_: integer;
  perplexity_loss_prob: real;
  perplexity_loss_mean_log: real;
  perplexity_loss_perp: real;
begin
  perplexity_loss_batch := Length(y_true);
  if perplexity_loss_batch <> Length(y_pred) then begin
  panic('Batch size of y_true and y_pred must be equal.');
end;
  perplexity_loss_sentence_len := Length(y_true[0]);
  if perplexity_loss_sentence_len <> Length(y_pred[0]) then begin
  panic('Sentence length of y_true and y_pred must be equal.');
end;
  perplexity_loss_vocab_size := Length(y_pred[0][0]);
  perplexity_loss_b := 0;
  perplexity_loss_total_perp := 0;
  while perplexity_loss_b < perplexity_loss_batch do begin
  if (Length(y_true[perplexity_loss_b]) <> perplexity_loss_sentence_len) or (Length(y_pred[perplexity_loss_b]) <> perplexity_loss_sentence_len) then begin
  panic('Sentence length of y_true and y_pred must be equal.');
end;
  perplexity_loss_sum_log := 0;
  perplexity_loss_j := 0;
  while perplexity_loss_j < perplexity_loss_sentence_len do begin
  perplexity_loss_label_ := y_true[perplexity_loss_b][perplexity_loss_j];
  if perplexity_loss_label_ >= perplexity_loss_vocab_size then begin
  panic('Label value must not be greater than vocabulary size.');
end;
  perplexity_loss_prob := clip(y_pred[perplexity_loss_b][perplexity_loss_j][perplexity_loss_label_], epsilon, 1);
  perplexity_loss_sum_log := perplexity_loss_sum_log + ln(perplexity_loss_prob);
  perplexity_loss_j := perplexity_loss_j + 1;
end;
  perplexity_loss_mean_log := perplexity_loss_sum_log / to_float(perplexity_loss_sentence_len);
  perplexity_loss_perp := exp(-perplexity_loss_mean_log);
  perplexity_loss_total_perp := perplexity_loss_total_perp + perplexity_loss_perp;
  perplexity_loss_b := perplexity_loss_b + 1;
end;
  exit(perplexity_loss_total_perp / to_float(perplexity_loss_batch));
end;
function smooth_l1_loss(y_true: RealArray; y_pred: RealArray; beta: real): real;
var
  smooth_l1_loss_total: real;
  smooth_l1_loss_i: integer;
  smooth_l1_loss_diff: real;
begin
  if Length(y_true) <> Length(y_pred) then begin
  panic('The length of the two arrays should be the same.');
end;
  smooth_l1_loss_total := 0;
  smooth_l1_loss_i := 0;
  while smooth_l1_loss_i < Length(y_true) do begin
  smooth_l1_loss_diff := absf(y_true[smooth_l1_loss_i] - y_pred[smooth_l1_loss_i]);
  if smooth_l1_loss_diff < beta then begin
  smooth_l1_loss_total := smooth_l1_loss_total + (((0.5 * smooth_l1_loss_diff) * smooth_l1_loss_diff) / beta);
end else begin
  smooth_l1_loss_total := (smooth_l1_loss_total + smooth_l1_loss_diff) - (0.5 * beta);
end;
  smooth_l1_loss_i := smooth_l1_loss_i + 1;
end;
  exit(smooth_l1_loss_total / to_float(Length(y_true)));
end;
function kullback_leibler_divergence(y_true: RealArray; y_pred: RealArray): real;
var
  kullback_leibler_divergence_total: real;
  kullback_leibler_divergence_i: integer;
begin
  if Length(y_true) <> Length(y_pred) then begin
  panic('Input arrays must have the same length.');
end;
  kullback_leibler_divergence_total := 0;
  kullback_leibler_divergence_i := 0;
  while kullback_leibler_divergence_i < Length(y_true) do begin
  kullback_leibler_divergence_total := kullback_leibler_divergence_total + (y_true[kullback_leibler_divergence_i] * ln(y_true[kullback_leibler_divergence_i] / y_pred[kullback_leibler_divergence_i]));
  kullback_leibler_divergence_i := kullback_leibler_divergence_i + 1;
end;
  exit(kullback_leibler_divergence_total);
end;
procedure main();
var
  main_y_true_bc: array of real;
  main_y_pred_bc: array of real;
  main_y_true_cce: array of array of real;
  main_y_pred_cce: array of array of real;
  main_alpha: array of real;
  main_y_true_hinge: array of real;
  main_y_pred_hinge: array of real;
  main_y_true_huber: array of real;
  main_y_pred_huber: array of real;
  main_y_true_mape: array of real;
  main_y_pred_mape: array of real;
  main_y_true_perp: array of array of integer;
  main_y_pred_perp: array of array of array of real;
  main_y_true_smooth: array of real;
  main_y_pred_smooth: array of real;
  main_y_true_kl: array of real;
  main_y_pred_kl: array of real;
begin
  main_y_true_bc := [0, 1, 1, 0, 1];
  main_y_pred_bc := [0.2, 0.7, 0.9, 0.3, 0.8];
  writeln(binary_cross_entropy(main_y_true_bc, main_y_pred_bc, 1e-15));
  writeln(binary_focal_cross_entropy(main_y_true_bc, main_y_pred_bc, 2, 0.25, 1e-15));
  main_y_true_cce := [[1, 0, 0], [0, 1, 0], [0, 0, 1]];
  main_y_pred_cce := [[0.9, 0.1, 0], [0.2, 0.7, 0.1], [0, 0.1, 0.9]];
  writeln(categorical_cross_entropy(main_y_true_cce, main_y_pred_cce, 1e-15));
  main_alpha := [0.6, 0.2, 0.7];
  writeln(categorical_focal_cross_entropy(main_y_true_cce, main_y_pred_cce, main_alpha, 2, 1e-15));
  main_y_true_hinge := [-1, 1, 1, -1, 1];
  main_y_pred_hinge := [-4, -0.3, 0.7, 5, 10];
  writeln(hinge_loss(main_y_true_hinge, main_y_pred_hinge));
  main_y_true_huber := [0.9, 10, 2, 1, 5.2];
  main_y_pred_huber := [0.8, 2.1, 2.9, 4.2, 5.2];
  writeln(huber_loss(main_y_true_huber, main_y_pred_huber, 1));
  writeln(mean_squared_error(main_y_true_huber, main_y_pred_huber));
  writeln(mean_absolute_error(main_y_true_huber, main_y_pred_huber));
  writeln(mean_squared_logarithmic_error(main_y_true_huber, main_y_pred_huber));
  main_y_true_mape := [10, 20, 30, 40];
  main_y_pred_mape := [12, 18, 33, 45];
  writeln(mean_absolute_percentage_error(main_y_true_mape, main_y_pred_mape, 1e-15));
  main_y_true_perp := [[1, 4], [2, 3]];
  main_y_pred_perp := [[[0.28, 0.19, 0.21, 0.15, 0.17], [0.24, 0.19, 0.09, 0.18, 0.3]], [[0.03, 0.26, 0.21, 0.18, 0.32], [0.28, 0.1, 0.33, 0.15, 0.14]]];
  writeln(perplexity_loss(main_y_true_perp, main_y_pred_perp, 1e-07));
  main_y_true_smooth := [3, 5, 2, 7];
  main_y_pred_smooth := [2.9, 4.8, 2.1, 7.2];
  writeln(smooth_l1_loss(main_y_true_smooth, main_y_pred_smooth, 1));
  main_y_true_kl := [0.2, 0.3, 0.5];
  main_y_pred_kl := [0.3, 0.3, 0.4];
  writeln(kullback_leibler_divergence(main_y_true_kl, main_y_pred_kl));
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
