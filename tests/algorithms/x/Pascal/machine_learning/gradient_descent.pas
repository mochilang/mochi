{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type DataPoint = record
  x: array of real;
  y: real;
end;
type RealArray = array of real;
type DataPointArray = array of DataPoint;
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
  train_data: array of DataPoint;
  test_data: array of DataPoint;
  parameter_vector: array of real;
  a: RealArray;
  atol: real;
  b: RealArray;
  data: DataPointArray;
  dp: DataPoint;
  index: integer;
  initial_params: RealArray;
  input: RealArray;
  params: RealArray;
  rtol: real;
  x: real;
function makeDataPoint(x: RealArray; y: real): DataPoint; forward;
function absf(x: real): real; forward;
function hypothesis_value(input: RealArray; params: RealArray): real; forward;
function calc_error(dp: DataPoint; params: RealArray): real; forward;
function summation_of_cost_derivative(index: integer; params: RealArray; data: DataPointArray): real; forward;
function get_cost_derivative(index: integer; params: RealArray; data: DataPointArray): real; forward;
function allclose(a: RealArray; b: RealArray; atol: real; rtol: real): boolean; forward;
function run_gradient_descent(train_data: DataPointArray; initial_params: RealArray): RealArray; forward;
procedure test_gradient_descent(test_data: DataPointArray; params: RealArray); forward;
function makeDataPoint(x: RealArray; y: real): DataPoint;
begin
  Result.x := x;
  Result.y := y;
end;
function absf(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function hypothesis_value(input: RealArray; params: RealArray): real;
var
  hypothesis_value_value: real;
  hypothesis_value_i: integer;
begin
  hypothesis_value_value := params[0];
  hypothesis_value_i := 0;
  while hypothesis_value_i < Length(input) do begin
  hypothesis_value_value := hypothesis_value_value + (input[hypothesis_value_i] * params[hypothesis_value_i + 1]);
  hypothesis_value_i := hypothesis_value_i + 1;
end;
  exit(hypothesis_value_value);
end;
function calc_error(dp: DataPoint; params: RealArray): real;
begin
  exit(hypothesis_value(dp.x, params) - dp.y);
end;
function summation_of_cost_derivative(index: integer; params: RealArray; data: DataPointArray): real;
var
  summation_of_cost_derivative_sum: real;
  summation_of_cost_derivative_i: integer;
  summation_of_cost_derivative_dp: DataPoint;
  summation_of_cost_derivative_e: real;
begin
  summation_of_cost_derivative_sum := 0;
  summation_of_cost_derivative_i := 0;
  while summation_of_cost_derivative_i < Length(data) do begin
  summation_of_cost_derivative_dp := data[summation_of_cost_derivative_i];
  summation_of_cost_derivative_e := calc_error(summation_of_cost_derivative_dp, params);
  if index = -1 then begin
  summation_of_cost_derivative_sum := summation_of_cost_derivative_sum + summation_of_cost_derivative_e;
end else begin
  summation_of_cost_derivative_sum := summation_of_cost_derivative_sum + (summation_of_cost_derivative_e * summation_of_cost_derivative_dp.x[index]);
end;
  summation_of_cost_derivative_i := summation_of_cost_derivative_i + 1;
end;
  exit(summation_of_cost_derivative_sum);
end;
function get_cost_derivative(index: integer; params: RealArray; data: DataPointArray): real;
begin
  exit(summation_of_cost_derivative(index, params, data) / Double(Length(data)));
end;
function allclose(a: RealArray; b: RealArray; atol: real; rtol: real): boolean;
var
  allclose_i: integer;
  allclose_diff: real;
  allclose_limit: real;
begin
  allclose_i := 0;
  while allclose_i < Length(a) do begin
  allclose_diff := absf(a[allclose_i] - b[allclose_i]);
  allclose_limit := atol + (rtol * absf(b[allclose_i]));
  if allclose_diff > allclose_limit then begin
  exit(false);
end;
  allclose_i := allclose_i + 1;
end;
  exit(true);
end;
function run_gradient_descent(train_data: DataPointArray; initial_params: RealArray): RealArray;
var
  run_gradient_descent_learning_rate: real;
  run_gradient_descent_absolute_error_limit: real;
  run_gradient_descent_relative_error_limit: real;
  run_gradient_descent_j: integer;
  run_gradient_descent_params: array of real;
  run_gradient_descent_temp: array of real;
  run_gradient_descent_i: integer;
  run_gradient_descent_deriv: real;
begin
  run_gradient_descent_learning_rate := 0.009;
  run_gradient_descent_absolute_error_limit := 2e-06;
  run_gradient_descent_relative_error_limit := 0;
  run_gradient_descent_j := 0;
  run_gradient_descent_params := initial_params;
  while true do begin
  run_gradient_descent_j := run_gradient_descent_j + 1;
  run_gradient_descent_temp := [];
  run_gradient_descent_i := 0;
  while run_gradient_descent_i < Length(run_gradient_descent_params) do begin
  run_gradient_descent_deriv := get_cost_derivative(run_gradient_descent_i - 1, run_gradient_descent_params, train_data);
  run_gradient_descent_temp := concat(run_gradient_descent_temp, [run_gradient_descent_params[run_gradient_descent_i] - (run_gradient_descent_learning_rate * run_gradient_descent_deriv)]);
  run_gradient_descent_i := run_gradient_descent_i + 1;
end;
  if allclose(run_gradient_descent_params, run_gradient_descent_temp, run_gradient_descent_absolute_error_limit, run_gradient_descent_relative_error_limit) then begin
  writeln('Number of iterations:' + IntToStr(run_gradient_descent_j));
  break;
end;
  run_gradient_descent_params := run_gradient_descent_temp;
end;
  exit(run_gradient_descent_params);
end;
procedure test_gradient_descent(test_data: DataPointArray; params: RealArray);
var
  test_gradient_descent_i: integer;
  test_gradient_descent_dp: DataPoint;
begin
  test_gradient_descent_i := 0;
  while test_gradient_descent_i < Length(test_data) do begin
  test_gradient_descent_dp := test_data[test_gradient_descent_i];
  writeln('Actual output value:' + FloatToStr(test_gradient_descent_dp.y));
  writeln('Hypothesis output:' + FloatToStr(hypothesis_value(test_gradient_descent_dp.x, params)));
  test_gradient_descent_i := test_gradient_descent_i + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  train_data := [makeDataPoint([5, 2, 3], 15), makeDataPoint([6, 5, 9], 25), makeDataPoint([11, 12, 13], 41), makeDataPoint([1, 1, 1], 8), makeDataPoint([11, 12, 13], 41)];
  test_data := [makeDataPoint([515, 22, 13], 555), makeDataPoint([61, 35, 49], 150)];
  parameter_vector := [2, 4, 1, 5];
  parameter_vector := run_gradient_descent(train_data, parameter_vector);
  writeln('' + #10 + 'Testing gradient descent for a linear hypothesis function.' + #10 + '');
  test_gradient_descent(test_data, parameter_vector);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
