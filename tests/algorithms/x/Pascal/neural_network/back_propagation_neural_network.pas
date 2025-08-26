{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
type Layer = record
  units: int64;
  weight: array of RealArray;
  bias: array of real;
  output: array of real;
  xdata: array of real;
  learn_rate: real;
end;
type Data = record
  x: array of RealArray;
  y: array of RealArray;
end;
type LayerArray = array of Layer;
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
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
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
  seed: int64;
function makeData(x: RealArrayArray; y: RealArrayArray): Data; forward;
function makeLayer(units: int64; weight: RealArrayArray; bias: RealArray; output: RealArray; xdata: RealArray; learn_rate: real): Layer; forward;
function rand(): int64; forward;
function random(): real; forward;
function expApprox(expApprox_x: real): real; forward;
function sigmoid(sigmoid_z: real): real; forward;
function sigmoid_vec(sigmoid_vec_v: RealArray): RealArray; forward;
function sigmoid_derivative(sigmoid_derivative_out_: RealArray): RealArray; forward;
function random_vector(random_vector_n: int64): RealArray; forward;
function random_matrix(random_matrix_r: int64; random_matrix_c: int64): RealArrayArray; forward;
function matvec(matvec_mat: RealArrayArray; matvec_vec: RealArray): RealArray; forward;
function matTvec(matTvec_mat: RealArrayArray; matTvec_vec: RealArray): RealArray; forward;
function vec_sub(vec_sub_a: RealArray; vec_sub_b: RealArray): RealArray; forward;
function vec_mul(vec_mul_a: RealArray; vec_mul_b: RealArray): RealArray; forward;
function vec_scalar_mul(vec_scalar_mul_v: RealArray; vec_scalar_mul_s: real): RealArray; forward;
function outer(outer_a: RealArray; outer_b: RealArray): RealArrayArray; forward;
function mat_scalar_mul(mat_scalar_mul_mat: RealArrayArray; mat_scalar_mul_s: real): RealArrayArray; forward;
function mat_sub(mat_sub_a: RealArrayArray; mat_sub_b: RealArrayArray): RealArrayArray; forward;
function init_layer(init_layer_units: int64; init_layer_back_units: int64; init_layer_lr: real): Layer; forward;
function forward(forward_layers: LayerArray; forward_x: RealArray): LayerArray; forward;
function backward(backward_layers: LayerArray; backward_grad: RealArray): LayerArray; forward;
function calc_loss(calc_loss_y: RealArray; calc_loss_yhat: RealArray): real; forward;
function calc_gradient(calc_gradient_y: RealArray; calc_gradient_yhat: RealArray): RealArray; forward;
function train(train_layers: LayerArray; train_xdata: RealArrayArray; train_ydata: RealArrayArray; train_rounds: int64; train_acc: real): real; forward;
function create_data(): Data; forward;
procedure main(); forward;
function makeData(x: RealArrayArray; y: RealArrayArray): Data;
begin
  Result.x := x;
  Result.y := y;
end;
function makeLayer(units: int64; weight: RealArrayArray; bias: RealArray; output: RealArray; xdata: RealArray; learn_rate: real): Layer;
begin
  Result.units := units;
  Result.weight := weight;
  Result.bias := bias;
  Result.output := output;
  Result.xdata := xdata;
  Result.learn_rate := learn_rate;
end;
function rand(): int64;
begin
  seed := ((seed * 1103515245) + 12345) mod 2147483648;
  exit(seed);
end;
function random(): real;
begin
  exit((1 * rand()) / 2.147483648e+09);
end;
function expApprox(expApprox_x: real): real;
var
  expApprox_y: real;
  expApprox_is_neg: boolean;
  expApprox_term: real;
  expApprox_sum: real;
  expApprox_n: int64;
begin
  expApprox_y := expApprox_x;
  expApprox_is_neg := false;
  if expApprox_x < 0 then begin
  expApprox_is_neg := true;
  expApprox_y := -expApprox_x;
end;
  expApprox_term := 1;
  expApprox_sum := 1;
  expApprox_n := 1;
  while expApprox_n < 30 do begin
  expApprox_term := (expApprox_term * expApprox_y) / Double(expApprox_n);
  expApprox_sum := expApprox_sum + expApprox_term;
  expApprox_n := expApprox_n + 1;
end;
  if expApprox_is_neg then begin
  exit(1 / expApprox_sum);
end;
  exit(expApprox_sum);
end;
function sigmoid(sigmoid_z: real): real;
begin
  exit(1 / (1 + expApprox(-sigmoid_z)));
end;
function sigmoid_vec(sigmoid_vec_v: RealArray): RealArray;
var
  sigmoid_vec_res: array of real;
  sigmoid_vec_i: int64;
begin
  sigmoid_vec_res := [];
  sigmoid_vec_i := 0;
  while sigmoid_vec_i < Length(sigmoid_vec_v) do begin
  sigmoid_vec_res := concat(sigmoid_vec_res, [sigmoid(sigmoid_vec_v[sigmoid_vec_i])]);
  sigmoid_vec_i := sigmoid_vec_i + 1;
end;
  exit(sigmoid_vec_res);
end;
function sigmoid_derivative(sigmoid_derivative_out_: RealArray): RealArray;
var
  sigmoid_derivative_res: array of real;
  sigmoid_derivative_i: int64;
  sigmoid_derivative_val: real;
begin
  sigmoid_derivative_res := [];
  sigmoid_derivative_i := 0;
  while sigmoid_derivative_i < Length(sigmoid_derivative_out_) do begin
  sigmoid_derivative_val := sigmoid_derivative_out_[sigmoid_derivative_i];
  sigmoid_derivative_res := concat(sigmoid_derivative_res, [sigmoid_derivative_val * (1 - sigmoid_derivative_val)]);
  sigmoid_derivative_i := sigmoid_derivative_i + 1;
end;
  exit(sigmoid_derivative_res);
end;
function random_vector(random_vector_n: int64): RealArray;
var
  random_vector_v: array of real;
  random_vector_i: int64;
begin
  random_vector_v := [];
  random_vector_i := 0;
  while random_vector_i < random_vector_n do begin
  random_vector_v := concat(random_vector_v, [random() - 0.5]);
  random_vector_i := random_vector_i + 1;
end;
  exit(random_vector_v);
end;
function random_matrix(random_matrix_r: int64; random_matrix_c: int64): RealArrayArray;
var
  random_matrix_m: array of RealArray;
  random_matrix_i: int64;
begin
  random_matrix_m := [];
  random_matrix_i := 0;
  while random_matrix_i < random_matrix_r do begin
  random_matrix_m := concat(random_matrix_m, [random_vector(random_matrix_c)]);
  random_matrix_i := random_matrix_i + 1;
end;
  exit(random_matrix_m);
end;
function matvec(matvec_mat: RealArrayArray; matvec_vec: RealArray): RealArray;
var
  matvec_res: array of real;
  matvec_i: int64;
  matvec_s: real;
  matvec_j: int64;
begin
  matvec_res := [];
  matvec_i := 0;
  while matvec_i < Length(matvec_mat) do begin
  matvec_s := 0;
  matvec_j := 0;
  while matvec_j < Length(matvec_vec) do begin
  matvec_s := matvec_s + (matvec_mat[matvec_i][matvec_j] * matvec_vec[matvec_j]);
  matvec_j := matvec_j + 1;
end;
  matvec_res := concat(matvec_res, [matvec_s]);
  matvec_i := matvec_i + 1;
end;
  exit(matvec_res);
end;
function matTvec(matTvec_mat: RealArrayArray; matTvec_vec: RealArray): RealArray;
var
  matTvec_cols: integer;
  matTvec_res: array of real;
  matTvec_j: int64;
  matTvec_s: real;
  matTvec_i: int64;
begin
  matTvec_cols := Length(matTvec_mat[0]);
  matTvec_res := [];
  matTvec_j := 0;
  while matTvec_j < matTvec_cols do begin
  matTvec_s := 0;
  matTvec_i := 0;
  while matTvec_i < Length(matTvec_mat) do begin
  matTvec_s := matTvec_s + (matTvec_mat[matTvec_i][matTvec_j] * matTvec_vec[matTvec_i]);
  matTvec_i := matTvec_i + 1;
end;
  matTvec_res := concat(matTvec_res, [matTvec_s]);
  matTvec_j := matTvec_j + 1;
end;
  exit(matTvec_res);
end;
function vec_sub(vec_sub_a: RealArray; vec_sub_b: RealArray): RealArray;
var
  vec_sub_res: array of real;
  vec_sub_i: int64;
begin
  vec_sub_res := [];
  vec_sub_i := 0;
  while vec_sub_i < Length(vec_sub_a) do begin
  vec_sub_res := concat(vec_sub_res, [vec_sub_a[vec_sub_i] - vec_sub_b[vec_sub_i]]);
  vec_sub_i := vec_sub_i + 1;
end;
  exit(vec_sub_res);
end;
function vec_mul(vec_mul_a: RealArray; vec_mul_b: RealArray): RealArray;
var
  vec_mul_res: array of real;
  vec_mul_i: int64;
begin
  vec_mul_res := [];
  vec_mul_i := 0;
  while vec_mul_i < Length(vec_mul_a) do begin
  vec_mul_res := concat(vec_mul_res, [vec_mul_a[vec_mul_i] * vec_mul_b[vec_mul_i]]);
  vec_mul_i := vec_mul_i + 1;
end;
  exit(vec_mul_res);
end;
function vec_scalar_mul(vec_scalar_mul_v: RealArray; vec_scalar_mul_s: real): RealArray;
var
  vec_scalar_mul_res: array of real;
  vec_scalar_mul_i: int64;
begin
  vec_scalar_mul_res := [];
  vec_scalar_mul_i := 0;
  while vec_scalar_mul_i < Length(vec_scalar_mul_v) do begin
  vec_scalar_mul_res := concat(vec_scalar_mul_res, [vec_scalar_mul_v[vec_scalar_mul_i] * vec_scalar_mul_s]);
  vec_scalar_mul_i := vec_scalar_mul_i + 1;
end;
  exit(vec_scalar_mul_res);
end;
function outer(outer_a: RealArray; outer_b: RealArray): RealArrayArray;
var
  outer_res: array of RealArray;
  outer_i: int64;
  outer_row: array of real;
  outer_j: int64;
begin
  outer_res := [];
  outer_i := 0;
  while outer_i < Length(outer_a) do begin
  outer_row := [];
  outer_j := 0;
  while outer_j < Length(outer_b) do begin
  outer_row := concat(outer_row, [outer_a[outer_i] * outer_b[outer_j]]);
  outer_j := outer_j + 1;
end;
  outer_res := concat(outer_res, [outer_row]);
  outer_i := outer_i + 1;
end;
  exit(outer_res);
end;
function mat_scalar_mul(mat_scalar_mul_mat: RealArrayArray; mat_scalar_mul_s: real): RealArrayArray;
var
  mat_scalar_mul_res: array of RealArray;
  mat_scalar_mul_i: int64;
  mat_scalar_mul_row: array of real;
  mat_scalar_mul_j: int64;
begin
  mat_scalar_mul_res := [];
  mat_scalar_mul_i := 0;
  while mat_scalar_mul_i < Length(mat_scalar_mul_mat) do begin
  mat_scalar_mul_row := [];
  mat_scalar_mul_j := 0;
  while mat_scalar_mul_j < Length(mat_scalar_mul_mat[mat_scalar_mul_i]) do begin
  mat_scalar_mul_row := concat(mat_scalar_mul_row, [mat_scalar_mul_mat[mat_scalar_mul_i][mat_scalar_mul_j] * mat_scalar_mul_s]);
  mat_scalar_mul_j := mat_scalar_mul_j + 1;
end;
  mat_scalar_mul_res := concat(mat_scalar_mul_res, [mat_scalar_mul_row]);
  mat_scalar_mul_i := mat_scalar_mul_i + 1;
end;
  exit(mat_scalar_mul_res);
end;
function mat_sub(mat_sub_a: RealArrayArray; mat_sub_b: RealArrayArray): RealArrayArray;
var
  mat_sub_res: array of RealArray;
  mat_sub_i: int64;
  mat_sub_row: array of real;
  mat_sub_j: int64;
begin
  mat_sub_res := [];
  mat_sub_i := 0;
  while mat_sub_i < Length(mat_sub_a) do begin
  mat_sub_row := [];
  mat_sub_j := 0;
  while mat_sub_j < Length(mat_sub_a[mat_sub_i]) do begin
  mat_sub_row := concat(mat_sub_row, [mat_sub_a[mat_sub_i][mat_sub_j] - mat_sub_b[mat_sub_i][mat_sub_j]]);
  mat_sub_j := mat_sub_j + 1;
end;
  mat_sub_res := concat(mat_sub_res, [mat_sub_row]);
  mat_sub_i := mat_sub_i + 1;
end;
  exit(mat_sub_res);
end;
function init_layer(init_layer_units: int64; init_layer_back_units: int64; init_layer_lr: real): Layer;
begin
  exit(makeLayer(init_layer_units, random_matrix(init_layer_units, init_layer_back_units), random_vector(init_layer_units), [], [], init_layer_lr));
end;
function forward(forward_layers: LayerArray; forward_x: RealArray): LayerArray;
var
  forward_data: array of real;
  forward_i: int64;
  forward_layer_var: Layer;
  forward_z: RealArray;
begin
  forward_data := forward_x;
  forward_i := 0;
  while forward_i < Length(forward_layers) do begin
  forward_layer_var := forward_layers[forward_i];
  forward_layer_var.xdata := forward_data;
  if forward_i = 0 then begin
  forward_layer_var.output := forward_data;
end else begin
  forward_z := vec_sub(matvec(forward_layer_var.weight, forward_data), forward_layer_var.bias);
  forward_layer_var.output := sigmoid_vec(forward_z);
  forward_data := forward_layer_var.output;
end;
  forward_layers[forward_i] := forward_layer_var;
  forward_i := forward_i + 1;
end;
  exit(forward_layers);
end;
function backward(backward_layers: LayerArray; backward_grad: RealArray): LayerArray;
var
  backward_g: array of real;
  backward_i: integer;
  backward_layer_var: Layer;
  backward_deriv: RealArray;
  backward_delta: RealArray;
  backward_grad_w: RealArrayArray;
begin
  backward_g := backward_grad;
  backward_i := Length(backward_layers) - 1;
  while backward_i > 0 do begin
  backward_layer_var := backward_layers[backward_i];
  backward_deriv := sigmoid_derivative(backward_layer_var.output);
  backward_delta := vec_mul(backward_g, backward_deriv);
  backward_grad_w := outer(backward_delta, backward_layer_var.xdata);
  backward_layer_var.weight := mat_sub(backward_layer_var.weight, mat_scalar_mul(backward_grad_w, backward_layer_var.learn_rate));
  backward_layer_var.bias := vec_sub(backward_layer_var.bias, vec_scalar_mul(backward_delta, backward_layer_var.learn_rate));
  backward_g := matTvec(backward_layer_var.weight, backward_delta);
  backward_layers[backward_i] := backward_layer_var;
  backward_i := backward_i - 1;
end;
  exit(backward_layers);
end;
function calc_loss(calc_loss_y: RealArray; calc_loss_yhat: RealArray): real;
var
  calc_loss_s: real;
  calc_loss_i: int64;
  calc_loss_d: real;
begin
  calc_loss_s := 0;
  calc_loss_i := 0;
  while calc_loss_i < Length(calc_loss_y) do begin
  calc_loss_d := calc_loss_y[calc_loss_i] - calc_loss_yhat[calc_loss_i];
  calc_loss_s := calc_loss_s + (calc_loss_d * calc_loss_d);
  calc_loss_i := calc_loss_i + 1;
end;
  exit(calc_loss_s);
end;
function calc_gradient(calc_gradient_y: RealArray; calc_gradient_yhat: RealArray): RealArray;
var
  calc_gradient_g: array of real;
  calc_gradient_i: int64;
begin
  calc_gradient_g := [];
  calc_gradient_i := 0;
  while calc_gradient_i < Length(calc_gradient_y) do begin
  calc_gradient_g := concat(calc_gradient_g, [2 * (calc_gradient_yhat[calc_gradient_i] - calc_gradient_y[calc_gradient_i])]);
  calc_gradient_i := calc_gradient_i + 1;
end;
  exit(calc_gradient_g);
end;
function train(train_layers: LayerArray; train_xdata: RealArrayArray; train_ydata: RealArrayArray; train_rounds: int64; train_acc: real): real;
var
  train_r: int64;
  train_i: int64;
  train_out_: array of real;
  train_grad: RealArray;
begin
  train_r := 0;
  while train_r < train_rounds do begin
  train_i := 0;
  while train_i < Length(train_xdata) do begin
  train_layers := forward(train_layers, train_xdata[train_i]);
  train_out_ := train_layers[Length(train_layers) - 1].output;
  train_grad := calc_gradient(train_ydata[train_i], train_out_);
  train_layers := backward(train_layers, train_grad);
  train_i := train_i + 1;
end;
  train_r := train_r + 1;
end;
  exit(0);
end;
function create_data(): Data;
var
  create_data_x: array of RealArray;
  create_data_i: int64;
  create_data_y: array of RealArray;
begin
  create_data_x := [];
  create_data_i := 0;
  while create_data_i < 10 do begin
  create_data_x := concat(create_data_x, [random_vector(10)]);
  create_data_i := create_data_i + 1;
end;
  create_data_y := [[0.8, 0.4], [0.4, 0.3], [0.34, 0.45], [0.67, 0.32], [0.88, 0.67], [0.78, 0.77], [0.55, 0.66], [0.55, 0.43], [0.54, 0.1], [0.1, 0.5]];
  exit(makeData(create_data_x, create_data_y));
end;
procedure main();
var
  main_data_var: Data;
  main_x: array of RealArray;
  main_y: array of RealArray;
  main_layers: array of Layer;
  main_final_mse: real;
begin
  main_data_var := create_data();
  main_x := main_data_var.x;
  main_y := main_data_var.y;
  main_layers := [];
  main_layers := concat(main_layers, [init_layer(10, 0, 0.3)]);
  main_layers := concat(main_layers, [init_layer(20, 10, 0.3)]);
  main_layers := concat(main_layers, [init_layer(30, 20, 0.3)]);
  main_layers := concat(main_layers, [init_layer(2, 30, 0.3)]);
  main_final_mse := train(main_layers, main_x, main_y, 100, 0.01);
  writeln(main_final_mse);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  seed := 1;
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
