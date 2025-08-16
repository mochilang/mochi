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
  data_x: array of RealArray;
  data_y: array of real;
  theta: RealArray;
  i: integer;
  predicted_y: array of real;
  original_y: array of real;
  mae: real;
  y: RealArray;
  len_data: integer;
  alpha: real;
  x: real;
function dot(x: RealArray; y: RealArray): real; forward;
function run_steep_gradient_descent(data_x: RealArrayArray; data_y: RealArray; len_data: integer; alpha: real; theta: RealArray): RealArray; forward;
function sum_of_square_error(data_x: RealArrayArray; data_y: RealArray; len_data: integer; theta: RealArray): real; forward;
function run_linear_regression(data_x: RealArrayArray; data_y: RealArray): RealArray; forward;
function absf(x: real): real; forward;
function mean_absolute_error(predicted_y: RealArray; original_y: RealArray): real; forward;
function dot(x: RealArray; y: RealArray): real;
var
  dot_sum: real;
  dot_i: integer;
begin
  dot_sum := 0;
  dot_i := 0;
  while dot_i < Length(x) do begin
  dot_sum := dot_sum + (x[dot_i] * y[dot_i]);
  dot_i := dot_i + 1;
end;
  exit(dot_sum);
end;
function run_steep_gradient_descent(data_x: RealArrayArray; data_y: RealArray; len_data: integer; alpha: real; theta: RealArray): RealArray;
var
  run_steep_gradient_descent_gradients: array of real;
  run_steep_gradient_descent_j: integer;
  run_steep_gradient_descent_i: integer;
  run_steep_gradient_descent_prediction: real;
  run_steep_gradient_descent_error: real;
  run_steep_gradient_descent_k: integer;
  run_steep_gradient_descent_t: array of real;
  run_steep_gradient_descent_g: integer;
begin
  run_steep_gradient_descent_gradients := [];
  run_steep_gradient_descent_j := 0;
  while run_steep_gradient_descent_j < Length(theta) do begin
  run_steep_gradient_descent_gradients := concat(run_steep_gradient_descent_gradients, [0]);
  run_steep_gradient_descent_j := run_steep_gradient_descent_j + 1;
end;
  run_steep_gradient_descent_i := 0;
  while run_steep_gradient_descent_i < len_data do begin
  run_steep_gradient_descent_prediction := dot(theta, data_x[run_steep_gradient_descent_i]);
  run_steep_gradient_descent_error := run_steep_gradient_descent_prediction - data_y[run_steep_gradient_descent_i];
  run_steep_gradient_descent_k := 0;
  while run_steep_gradient_descent_k < Length(theta) do begin
  run_steep_gradient_descent_gradients[run_steep_gradient_descent_k] := run_steep_gradient_descent_gradients[run_steep_gradient_descent_k] + (run_steep_gradient_descent_error * data_x[run_steep_gradient_descent_i][run_steep_gradient_descent_k]);
  run_steep_gradient_descent_k := run_steep_gradient_descent_k + 1;
end;
  run_steep_gradient_descent_i := run_steep_gradient_descent_i + 1;
end;
  run_steep_gradient_descent_t := [];
  run_steep_gradient_descent_g := 0;
  while run_steep_gradient_descent_g < Length(theta) do begin
  run_steep_gradient_descent_t := concat(run_steep_gradient_descent_t, [theta[run_steep_gradient_descent_g] - ((alpha / len_data) * run_steep_gradient_descent_gradients[run_steep_gradient_descent_g])]);
  run_steep_gradient_descent_g := run_steep_gradient_descent_g + 1;
end;
  exit(run_steep_gradient_descent_t);
end;
function sum_of_square_error(data_x: RealArrayArray; data_y: RealArray; len_data: integer; theta: RealArray): real;
var
  sum_of_square_error_total: real;
  sum_of_square_error_i: integer;
  sum_of_square_error_prediction: real;
  sum_of_square_error_diff: real;
begin
  sum_of_square_error_total := 0;
  sum_of_square_error_i := 0;
  while sum_of_square_error_i < len_data do begin
  sum_of_square_error_prediction := dot(theta, data_x[sum_of_square_error_i]);
  sum_of_square_error_diff := sum_of_square_error_prediction - data_y[sum_of_square_error_i];
  sum_of_square_error_total := sum_of_square_error_total + (sum_of_square_error_diff * sum_of_square_error_diff);
  sum_of_square_error_i := sum_of_square_error_i + 1;
end;
  exit(sum_of_square_error_total / (2 * len_data));
end;
function run_linear_regression(data_x: RealArrayArray; data_y: RealArray): RealArray;
var
  run_linear_regression_iterations: integer;
  run_linear_regression_alpha: real;
  run_linear_regression_no_features: integer;
  run_linear_regression_len_data: integer;
  run_linear_regression_theta: array of real;
  run_linear_regression_i: integer;
  run_linear_regression_iter: integer;
  run_linear_regression_error: real;
begin
  run_linear_regression_iterations := 10;
  run_linear_regression_alpha := 0.01;
  run_linear_regression_no_features := Length(data_x[0]);
  run_linear_regression_len_data := Length(data_x);
  run_linear_regression_theta := [];
  run_linear_regression_i := 0;
  while run_linear_regression_i < run_linear_regression_no_features do begin
  run_linear_regression_theta := concat(run_linear_regression_theta, [0]);
  run_linear_regression_i := run_linear_regression_i + 1;
end;
  run_linear_regression_iter := 0;
  while run_linear_regression_iter < run_linear_regression_iterations do begin
  run_linear_regression_theta := run_steep_gradient_descent(data_x, data_y, run_linear_regression_len_data, run_linear_regression_alpha, run_linear_regression_theta);
  run_linear_regression_error := sum_of_square_error(data_x, data_y, run_linear_regression_len_data, run_linear_regression_theta);
  writeln((('At Iteration ' + IntToStr(run_linear_regression_iter + 1)) + ' - Error is ') + FloatToStr(run_linear_regression_error));
  run_linear_regression_iter := run_linear_regression_iter + 1;
end;
  exit(run_linear_regression_theta);
end;
function absf(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end else begin
  exit(x);
end;
end;
function mean_absolute_error(predicted_y: RealArray; original_y: RealArray): real;
var
  mean_absolute_error_total: real;
  mean_absolute_error_i: integer;
  mean_absolute_error_diff: real;
begin
  mean_absolute_error_total := 0;
  mean_absolute_error_i := 0;
  while mean_absolute_error_i < Length(predicted_y) do begin
  mean_absolute_error_diff := absf(predicted_y[mean_absolute_error_i] - original_y[mean_absolute_error_i]);
  mean_absolute_error_total := mean_absolute_error_total + mean_absolute_error_diff;
  mean_absolute_error_i := mean_absolute_error_i + 1;
end;
  exit(mean_absolute_error_total / Length(predicted_y));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  data_x := [[1, 1], [1, 2], [1, 3]];
  data_y := [1, 2, 3];
  theta := run_linear_regression(data_x, data_y);
  writeln('Resultant Feature vector :');
  i := 0;
  while i < Length(theta) do begin
  writeln(FloatToStr(theta[i]));
  i := i + 1;
end;
  predicted_y := [3, -0.5, 2, 7];
  original_y := [2.5, 0, 2, 8];
  mae := mean_absolute_error(predicted_y, original_y);
  writeln('Mean Absolute Error : ' + FloatToStr(mae));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
