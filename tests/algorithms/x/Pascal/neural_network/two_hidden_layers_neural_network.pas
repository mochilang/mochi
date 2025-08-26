{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
type Network = record
  w1: array of RealArray;
  w2: array of RealArray;
  w3: array of RealArray;
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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: int64): real;
begin
  _to_float := x;
end;
function to_float(x: int64): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function makeNetwork(w1: RealArrayArray; w2: RealArrayArray; w3: RealArrayArray): Network; forward;
function exp_approx(exp_approx_x: real): real; forward;
function sigmoid(sigmoid_x: real): real; forward;
function sigmoid_derivative(sigmoid_derivative_x: real): real; forward;
function new_network(): Network; forward;
function feedforward(feedforward_net: Network; feedforward_input: RealArray): real; forward;
procedure train(train_net: Network; train_inputs: RealArrayArray; train_outputs: RealArray; train_iterations: int64); forward;
function predict(predict_net: Network; predict_input: RealArray): int64; forward;
function example(): int64; forward;
procedure main(); forward;
function makeNetwork(w1: RealArrayArray; w2: RealArrayArray; w3: RealArrayArray): Network;
begin
  Result.w1 := w1;
  Result.w2 := w2;
  Result.w3 := w3;
end;
function exp_approx(exp_approx_x: real): real;
var
  exp_approx_sum: real;
  exp_approx_term: real;
  exp_approx_i: int64;
begin
  exp_approx_sum := 1;
  exp_approx_term := 1;
  exp_approx_i := 1;
  while exp_approx_i < 10 do begin
  exp_approx_term := (exp_approx_term * exp_approx_x) / Double(exp_approx_i);
  exp_approx_sum := exp_approx_sum + exp_approx_term;
  exp_approx_i := exp_approx_i + 1;
end;
  exit(exp_approx_sum);
end;
function sigmoid(sigmoid_x: real): real;
begin
  exit(1 / (1 + exp_approx(-sigmoid_x)));
end;
function sigmoid_derivative(sigmoid_derivative_x: real): real;
begin
  exit(sigmoid_derivative_x * (1 - sigmoid_derivative_x));
end;
function new_network(): Network;
begin
  exit(makeNetwork([[0.1, 0.2, 0.3, 0.4], [0.5, 0.6, 0.7, 0.8], [0.9, 1, 1.1, 1.2]], [[0.1, 0.2, 0.3], [0.4, 0.5, 0.6], [0.7, 0.8, 0.9], [1, 1.1, 1.2]], [[0.1], [0.2], [0.3]]));
end;
function feedforward(feedforward_net: Network; feedforward_input: RealArray): real;
var
  feedforward_hidden1: array of real;
  feedforward_j: int64;
  feedforward_sum1: real;
  feedforward_i: int64;
  feedforward_hidden2: array of real;
  feedforward_k: int64;
  feedforward_sum2: real;
  feedforward_j2: int64;
  feedforward_sum3: real;
  feedforward_k2: int64;
  feedforward_out_: real;
begin
  feedforward_hidden1 := [];
  feedforward_j := 0;
  while feedforward_j < 4 do begin
  feedforward_sum1 := 0;
  feedforward_i := 0;
  while feedforward_i < 3 do begin
  feedforward_sum1 := feedforward_sum1 + (feedforward_input[feedforward_i] * feedforward_net.w1[feedforward_i][feedforward_j]);
  feedforward_i := feedforward_i + 1;
end;
  feedforward_hidden1 := concat(feedforward_hidden1, [sigmoid(feedforward_sum1)]);
  feedforward_j := feedforward_j + 1;
end;
  feedforward_hidden2 := [];
  feedforward_k := 0;
  while feedforward_k < 3 do begin
  feedforward_sum2 := 0;
  feedforward_j2 := 0;
  while feedforward_j2 < 4 do begin
  feedforward_sum2 := feedforward_sum2 + (feedforward_hidden1[feedforward_j2] * feedforward_net.w2[feedforward_j2][feedforward_k]);
  feedforward_j2 := feedforward_j2 + 1;
end;
  feedforward_hidden2 := concat(feedforward_hidden2, [sigmoid(feedforward_sum2)]);
  feedforward_k := feedforward_k + 1;
end;
  feedforward_sum3 := 0;
  feedforward_k2 := 0;
  while feedforward_k2 < 3 do begin
  feedforward_sum3 := feedforward_sum3 + (feedforward_hidden2[feedforward_k2] * feedforward_net.w3[feedforward_k2][0]);
  feedforward_k2 := feedforward_k2 + 1;
end;
  feedforward_out_ := sigmoid(feedforward_sum3);
  exit(feedforward_out_);
end;
procedure train(train_net: Network; train_inputs: RealArrayArray; train_outputs: RealArray; train_iterations: int64);
var
  train_iter: int64;
  train_s: int64;
  train_inp: array of real;
  train_target: real;
  train_hidden1: array of real;
  train_j: int64;
  train_sum1: real;
  train_i: int64;
  train_hidden2: array of real;
  train_k: int64;
  train_sum2: real;
  train_j2: int64;
  train_sum3: real;
  train_k3: int64;
  train_output: real;
  train_error: real;
  train_delta_output: real;
  train_new_w3: array of RealArray;
  train_k4: int64;
  train_w3row: array of real;
  train_delta_hidden2: array of real;
  train_k5: int64;
  train_row: array of real;
  train_dh2: real;
  train_new_w2: array of RealArray;
  train_w2row: array of real;
  train_k6: int64;
  train_delta_hidden1: array of real;
  train_sumdh: real;
  train_k7: int64;
  train_row2: array of real;
  train_new_w1: array of RealArray;
  train_i2: int64;
  train_w1row: array of real;
begin
  train_iter := 0;
  while train_iter < train_iterations do begin
  train_s := 0;
  while train_s < Length(train_inputs) do begin
  train_inp := train_inputs[train_s];
  train_target := train_outputs[train_s];
  train_hidden1 := [];
  train_j := 0;
  while train_j < 4 do begin
  train_sum1 := 0;
  train_i := 0;
  while train_i < 3 do begin
  train_sum1 := train_sum1 + (train_inp[train_i] * train_net.w1[train_i][train_j]);
  train_i := train_i + 1;
end;
  train_hidden1 := concat(train_hidden1, [sigmoid(train_sum1)]);
  train_j := train_j + 1;
end;
  train_hidden2 := [];
  train_k := 0;
  while train_k < 3 do begin
  train_sum2 := 0;
  train_j2 := 0;
  while train_j2 < 4 do begin
  train_sum2 := train_sum2 + (train_hidden1[train_j2] * train_net.w2[train_j2][train_k]);
  train_j2 := train_j2 + 1;
end;
  train_hidden2 := concat(train_hidden2, [sigmoid(train_sum2)]);
  train_k := train_k + 1;
end;
  train_sum3 := 0;
  train_k3 := 0;
  while train_k3 < 3 do begin
  train_sum3 := train_sum3 + (train_hidden2[train_k3] * train_net.w3[train_k3][0]);
  train_k3 := train_k3 + 1;
end;
  train_output := sigmoid(train_sum3);
  train_error := train_target - train_output;
  train_delta_output := train_error * sigmoid_derivative(train_output);
  train_new_w3 := [];
  train_k4 := 0;
  while train_k4 < 3 do begin
  train_w3row := train_net.w3[train_k4];
  train_w3row[0] := train_w3row[0] + (train_hidden2[train_k4] * train_delta_output);
  train_new_w3 := concat(train_new_w3, [train_w3row]);
  train_k4 := train_k4 + 1;
end;
  train_net.w3 := train_new_w3;
  train_delta_hidden2 := [];
  train_k5 := 0;
  while train_k5 < 3 do begin
  train_row := train_net.w3[train_k5];
  train_dh2 := (train_row[0] * train_delta_output) * sigmoid_derivative(train_hidden2[train_k5]);
  train_delta_hidden2 := concat(train_delta_hidden2, [train_dh2]);
  train_k5 := train_k5 + 1;
end;
  train_new_w2 := [];
  train_j := 0;
  while train_j < 4 do begin
  train_w2row := train_net.w2[train_j];
  train_k6 := 0;
  while train_k6 < 3 do begin
  train_w2row[train_k6] := train_w2row[train_k6] + (train_hidden1[train_j] * train_delta_hidden2[train_k6]);
  train_k6 := train_k6 + 1;
end;
  train_new_w2 := concat(train_new_w2, [train_w2row]);
  train_j := train_j + 1;
end;
  train_net.w2 := train_new_w2;
  train_delta_hidden1 := [];
  train_j := 0;
  while train_j < 4 do begin
  train_sumdh := 0;
  train_k7 := 0;
  while train_k7 < 3 do begin
  train_row2 := train_net.w2[train_j];
  train_sumdh := train_sumdh + (train_row2[train_k7] * train_delta_hidden2[train_k7]);
  train_k7 := train_k7 + 1;
end;
  train_delta_hidden1 := concat(train_delta_hidden1, [train_sumdh * sigmoid_derivative(train_hidden1[train_j])]);
  train_j := train_j + 1;
end;
  train_new_w1 := [];
  train_i2 := 0;
  while train_i2 < 3 do begin
  train_w1row := train_net.w1[train_i2];
  train_j := 0;
  while train_j < 4 do begin
  train_w1row[train_j] := train_w1row[train_j] + (train_inp[train_i2] * train_delta_hidden1[train_j]);
  train_j := train_j + 1;
end;
  train_new_w1 := concat(train_new_w1, [train_w1row]);
  train_i2 := train_i2 + 1;
end;
  train_net.w1 := train_new_w1;
  train_s := train_s + 1;
end;
  train_iter := train_iter + 1;
end;
end;
function predict(predict_net: Network; predict_input: RealArray): int64;
var
  predict_out_: real;
begin
  predict_out_ := feedforward(predict_net, predict_input);
  if predict_out_ > 0.6 then begin
  exit(1);
end;
  exit(0);
end;
function example(): int64;
var
  example_inputs: array of RealArray;
  example_outputs: array of real;
  example_net: Network;
  example_result_: int64;
begin
  example_inputs := [[0, 0, 0], [0, 0, 1], [0, 1, 0], [0, 1, 1], [1, 0, 0], [1, 0, 1], [1, 1, 0], [1, 1, 1]];
  example_outputs := [0, 1, 1, 0, 1, 0, 0, 1];
  example_net := new_network();
  train(example_net, example_inputs, example_outputs, 10);
  example_result_ := predict(example_net, [1, 1, 1]);
  writeln(IntToStr(example_result_));
  exit(example_result_);
end;
procedure main();
begin
  example();
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
  writeln('');
end.
