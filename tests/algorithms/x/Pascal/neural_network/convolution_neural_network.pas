{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Variants;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
type RealArrayArrayArray = array of RealArrayArray;
type CNN = record
  conv_kernels: array of RealArrayArray;
  conv_bias: array of real;
  conv_step: integer;
  pool_size: integer;
  w_hidden: array of RealArray;
  w_out: array of RealArray;
  b_hidden: array of real;
  b_out: array of real;
  rate_weight: real;
  rate_bias: real;
end;
type TrainSample = record
  image: array of RealArray;
  target: array of real;
end;
type TrainSampleArray = array of TrainSample;
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
  seed: integer;
  x: real;
  step: integer;
  a: RealArray;
  kernel: RealArrayArray;
  size: integer;
  data: RealArrayArray;
  maps: RealArrayArrayArray;
  samples: TrainSampleArray;
  m: RealArrayArray;
  v: RealArray;
  map: RealArrayArray;
  bias: real;
  b: RealArray;
  cnn_var: CNN;
  epochs: integer;
function makeTrainSample(image: RealArrayArray; target: RealArray): TrainSample; forward;
function makeCNN(conv_kernels: RealArrayArrayArray; conv_bias: RealArray; conv_step: integer; pool_size: integer; w_hidden: RealArrayArray; w_out: RealArrayArray; b_hidden: RealArray; b_out: RealArray; rate_weight: real; rate_bias: real): CNN; forward;
function random(): real; forward;
function sigmoid(x: real): real; forward;
function to_float(x: integer): real; forward;
function exp_(x: real): real; forward;
function convolve(data: RealArrayArray; kernel: RealArrayArray; step: integer; bias: real): RealArrayArray; forward;
function average_pool(map: RealArrayArray; size: integer): RealArrayArray; forward;
function flatten(maps: RealArrayArrayArray): RealArray; forward;
function vec_mul_mat(v: RealArray; m: RealArrayArray): RealArray; forward;
function matT_vec_mul(m: RealArrayArray; v: RealArray): RealArray; forward;
function vec_add(a: RealArray; b: RealArray): RealArray; forward;
function vec_sub(a: RealArray; b: RealArray): RealArray; forward;
function vec_mul(a: RealArray; b: RealArray): RealArray; forward;
function vec_map_sig(v: RealArray): RealArray; forward;
function new_cnn(): CNN; forward;
function forward(forward_cnn_var: CNN; data: RealArrayArray): RealArray; forward;
function train(forward_cnn_var: CNN; samples: TrainSampleArray; epochs: integer): CNN; forward;
procedure main(); forward;
function makeTrainSample(image: RealArrayArray; target: RealArray): TrainSample;
begin
  Result.image := image;
  Result.target := target;
end;
function makeCNN(conv_kernels: RealArrayArrayArray; conv_bias: RealArray; conv_step: integer; pool_size: integer; w_hidden: RealArrayArray; w_out: RealArrayArray; b_hidden: RealArray; b_out: RealArray; rate_weight: real; rate_bias: real): CNN;
begin
  Result.conv_kernels := conv_kernels;
  Result.conv_bias := conv_bias;
  Result.conv_step := conv_step;
  Result.pool_size := pool_size;
  Result.w_hidden := w_hidden;
  Result.w_out := w_out;
  Result.b_hidden := b_hidden;
  Result.b_out := b_out;
  Result.rate_weight := rate_weight;
  Result.rate_bias := rate_bias;
end;
function random(): real;
begin
  seed := ((seed * 13) + 7) mod 100;
  exit(Double(seed) / 100);
end;
function sigmoid(x: real): real;
begin
  exit(1 / (1 + exp(-x)));
end;
function to_float(x: integer): real;
begin
  exit(x * 1);
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
function convolve(data: RealArrayArray; kernel: RealArrayArray; step: integer; bias: real): RealArrayArray;
var
  convolve_size_data: integer;
  convolve_size_kernel: integer;
  convolve_out_: array of RealArray;
  convolve_i: integer;
  convolve_row: array of real;
  convolve_j: integer;
  convolve_sum: real;
  convolve_a: integer;
  convolve_b: integer;
begin
  convolve_size_data := Length(data);
  convolve_size_kernel := Length(kernel);
  convolve_out_ := [];
  convolve_i := 0;
  while convolve_i <= (convolve_size_data - convolve_size_kernel) do begin
  convolve_row := [];
  convolve_j := 0;
  while convolve_j <= (convolve_size_data - convolve_size_kernel) do begin
  convolve_sum := 0;
  convolve_a := 0;
  while convolve_a < convolve_size_kernel do begin
  convolve_b := 0;
  while convolve_b < convolve_size_kernel do begin
  convolve_sum := convolve_sum + (data[convolve_i + convolve_a][convolve_j + convolve_b] * kernel[convolve_a][convolve_b]);
  convolve_b := convolve_b + 1;
end;
  convolve_a := convolve_a + 1;
end;
  convolve_row := concat(convolve_row, [sigmoid(convolve_sum - bias)]);
  convolve_j := convolve_j + step;
end;
  convolve_out_ := concat(convolve_out_, [convolve_row]);
  convolve_i := convolve_i + step;
end;
  exit(convolve_out_);
end;
function average_pool(map: RealArrayArray; size: integer): RealArrayArray;
var
  average_pool_out_: array of RealArray;
  average_pool_i: integer;
  average_pool_row: array of real;
  average_pool_j: integer;
  average_pool_sum: real;
  average_pool_a: integer;
  average_pool_b: integer;
begin
  average_pool_out_ := [];
  average_pool_i := 0;
  while average_pool_i < Length(map) do begin
  average_pool_row := [];
  average_pool_j := 0;
  while average_pool_j < Length(map[average_pool_i]) do begin
  average_pool_sum := 0;
  average_pool_a := 0;
  while average_pool_a < size do begin
  average_pool_b := 0;
  while average_pool_b < size do begin
  average_pool_sum := average_pool_sum + map[average_pool_i + average_pool_a][average_pool_j + average_pool_b];
  average_pool_b := average_pool_b + 1;
end;
  average_pool_a := average_pool_a + 1;
end;
  average_pool_row := concat(average_pool_row, [average_pool_sum / Double(size * size)]);
  average_pool_j := average_pool_j + size;
end;
  average_pool_out_ := concat(average_pool_out_, [average_pool_row]);
  average_pool_i := average_pool_i + size;
end;
  exit(average_pool_out_);
end;
function flatten(maps: RealArrayArrayArray): RealArray;
var
  flatten_out_: array of real;
  flatten_i: integer;
  flatten_j: integer;
  flatten_k: integer;
begin
  flatten_out_ := [];
  flatten_i := 0;
  while flatten_i < Length(maps) do begin
  flatten_j := 0;
  while flatten_j < Length(maps[flatten_i]) do begin
  flatten_k := 0;
  while flatten_k < Length(maps[flatten_i][flatten_j]) do begin
  flatten_out_ := concat(flatten_out_, [maps[flatten_i][flatten_j][flatten_k]]);
  flatten_k := flatten_k + 1;
end;
  flatten_j := flatten_j + 1;
end;
  flatten_i := flatten_i + 1;
end;
  exit(flatten_out_);
end;
function vec_mul_mat(v: RealArray; m: RealArrayArray): RealArray;
var
  vec_mul_mat_cols: integer;
  vec_mul_mat_res: array of real;
  vec_mul_mat_j: integer;
  vec_mul_mat_sum: real;
  vec_mul_mat_i: integer;
begin
  vec_mul_mat_cols := Length(m[0]);
  vec_mul_mat_res := [];
  vec_mul_mat_j := 0;
  while vec_mul_mat_j < vec_mul_mat_cols do begin
  vec_mul_mat_sum := 0;
  vec_mul_mat_i := 0;
  while vec_mul_mat_i < Length(v) do begin
  vec_mul_mat_sum := vec_mul_mat_sum + (v[vec_mul_mat_i] * m[vec_mul_mat_i][vec_mul_mat_j]);
  vec_mul_mat_i := vec_mul_mat_i + 1;
end;
  vec_mul_mat_res := concat(vec_mul_mat_res, [vec_mul_mat_sum]);
  vec_mul_mat_j := vec_mul_mat_j + 1;
end;
  exit(vec_mul_mat_res);
end;
function matT_vec_mul(m: RealArrayArray; v: RealArray): RealArray;
var
  matT_vec_mul_res: array of real;
  matT_vec_mul_i: integer;
  matT_vec_mul_sum: real;
  matT_vec_mul_j: integer;
begin
  matT_vec_mul_res := [];
  matT_vec_mul_i := 0;
  while matT_vec_mul_i < Length(m) do begin
  matT_vec_mul_sum := 0;
  matT_vec_mul_j := 0;
  while matT_vec_mul_j < Length(m[matT_vec_mul_i]) do begin
  matT_vec_mul_sum := matT_vec_mul_sum + (m[matT_vec_mul_i][matT_vec_mul_j] * v[matT_vec_mul_j]);
  matT_vec_mul_j := matT_vec_mul_j + 1;
end;
  matT_vec_mul_res := concat(matT_vec_mul_res, [matT_vec_mul_sum]);
  matT_vec_mul_i := matT_vec_mul_i + 1;
end;
  exit(matT_vec_mul_res);
end;
function vec_add(a: RealArray; b: RealArray): RealArray;
var
  vec_add_res: array of real;
  vec_add_i: integer;
begin
  vec_add_res := [];
  vec_add_i := 0;
  while vec_add_i < Length(a) do begin
  vec_add_res := concat(vec_add_res, [a[vec_add_i] + b[vec_add_i]]);
  vec_add_i := vec_add_i + 1;
end;
  exit(vec_add_res);
end;
function vec_sub(a: RealArray; b: RealArray): RealArray;
var
  vec_sub_res: array of real;
  vec_sub_i: integer;
begin
  vec_sub_res := [];
  vec_sub_i := 0;
  while vec_sub_i < Length(a) do begin
  vec_sub_res := concat(vec_sub_res, [a[vec_sub_i] - b[vec_sub_i]]);
  vec_sub_i := vec_sub_i + 1;
end;
  exit(vec_sub_res);
end;
function vec_mul(a: RealArray; b: RealArray): RealArray;
var
  vec_mul_res: array of real;
  vec_mul_i: integer;
begin
  vec_mul_res := [];
  vec_mul_i := 0;
  while vec_mul_i < Length(a) do begin
  vec_mul_res := concat(vec_mul_res, [a[vec_mul_i] * b[vec_mul_i]]);
  vec_mul_i := vec_mul_i + 1;
end;
  exit(vec_mul_res);
end;
function vec_map_sig(v: RealArray): RealArray;
var
  vec_map_sig_res: array of real;
  vec_map_sig_i: integer;
begin
  vec_map_sig_res := [];
  vec_map_sig_i := 0;
  while vec_map_sig_i < Length(v) do begin
  vec_map_sig_res := concat(vec_map_sig_res, [sigmoid(v[vec_map_sig_i])]);
  vec_map_sig_i := vec_map_sig_i + 1;
end;
  exit(vec_map_sig_res);
end;
function new_cnn(): CNN;
var
  new_cnn_k1: array of array of real;
  new_cnn_k2: array of array of real;
  new_cnn_conv_kernels: array of array of array of real;
  new_cnn_conv_bias: array of real;
  new_cnn_conv_step: integer;
  new_cnn_pool_size: integer;
  new_cnn_input_size: integer;
  new_cnn_hidden_size: integer;
  new_cnn_output_size: integer;
  new_cnn_w_hidden: array of RealArray;
  new_cnn_i: integer;
  new_cnn_row: array of real;
  new_cnn_j: integer;
  new_cnn_w_out: array of RealArray;
  new_cnn_b_hidden: array of real;
  new_cnn_b_out: array of real;
begin
  new_cnn_k1 := [[1, 0], [0, 1]];
  new_cnn_k2 := [[0, 1], [1, 0]];
  new_cnn_conv_kernels := [new_cnn_k1, new_cnn_k2];
  new_cnn_conv_bias := [0, 0];
  new_cnn_conv_step := 2;
  new_cnn_pool_size := 2;
  new_cnn_input_size := 2;
  new_cnn_hidden_size := 2;
  new_cnn_output_size := 2;
  new_cnn_w_hidden := [];
  new_cnn_i := 0;
  while new_cnn_i < new_cnn_input_size do begin
  new_cnn_row := [];
  new_cnn_j := 0;
  while new_cnn_j < new_cnn_hidden_size do begin
  new_cnn_row := concat(new_cnn_row, [random() - 0.5]);
  new_cnn_j := new_cnn_j + 1;
end;
  new_cnn_w_hidden := concat(new_cnn_w_hidden, [new_cnn_row]);
  new_cnn_i := new_cnn_i + 1;
end;
  new_cnn_w_out := [];
  new_cnn_i := 0;
  while new_cnn_i < new_cnn_hidden_size do begin
  new_cnn_row := [];
  new_cnn_j := 0;
  while new_cnn_j < new_cnn_output_size do begin
  new_cnn_row := concat(new_cnn_row, [random() - 0.5]);
  new_cnn_j := new_cnn_j + 1;
end;
  new_cnn_w_out := concat(new_cnn_w_out, [new_cnn_row]);
  new_cnn_i := new_cnn_i + 1;
end;
  new_cnn_b_hidden := [0, 0];
  new_cnn_b_out := [0, 0];
  exit(makeCNN(new_cnn_conv_kernels, new_cnn_conv_bias, new_cnn_conv_step, new_cnn_pool_size, new_cnn_w_hidden, new_cnn_w_out, new_cnn_b_hidden, new_cnn_b_out, 0.2, 0.2));
end;
function forward(forward_cnn_var: CNN; data: RealArrayArray): RealArray;
var
  forward_maps: array of RealArrayArray;
  forward_i: integer;
  forward_conv_map: RealArrayArray;
  forward_pooled: RealArrayArray;
  forward_flat: RealArray;
  forward_hidden_net: RealArray;
  forward_hidden_out: RealArray;
  forward_out_net: RealArray;
  forward_out_: RealArray;
begin
  forward_maps := [];
  forward_i := 0;
  while forward_i < Length(cnn_var.conv_kernels) do begin
  forward_conv_map := convolve(data, cnn_var.conv_kernels[forward_i], cnn_var.conv_step, cnn_var.conv_bias[forward_i]);
  forward_pooled := average_pool(forward_conv_map, cnn_var.pool_size);
  forward_maps := concat(forward_maps, [forward_pooled]);
  forward_i := forward_i + 1;
end;
  forward_flat := flatten(forward_maps);
  forward_hidden_net := vec_add(vec_mul_mat(forward_flat, cnn_var.w_hidden), cnn_var.b_hidden);
  forward_hidden_out := vec_map_sig(forward_hidden_net);
  forward_out_net := vec_add(vec_mul_mat(forward_hidden_out, cnn_var.w_out), cnn_var.b_out);
  forward_out_ := vec_map_sig(forward_out_net);
  exit(forward_out_);
end;
function train(forward_cnn_var: CNN; samples: TrainSampleArray; epochs: integer): CNN;
var
  train_w_out: Variant;
  train_b_out: Variant;
  train_w_hidden: Variant;
  train_b_hidden: Variant;
  train_e: integer;
  train_s: integer;
  train_data: array of RealArray;
  train_target: array of real;
  train_maps: array of RealArrayArray;
  train_i: integer;
  train_conv_map: RealArrayArray;
  train_pooled: RealArrayArray;
  train_flat: RealArray;
  train_hidden_net: RealArray;
  train_hidden_out: RealArray;
  train_out_net: RealArray;
  train_out_: RealArray;
  train_error_out: RealArray;
  train_pd_out: RealArray;
  train_error_hidden: RealArray;
  train_pd_hidden: RealArray;
  train_j: integer;
  train_k: integer;
  train_i_h: integer;
  train_j_h: integer;
begin
  train_w_out := cnn_var.w_out;
  train_b_out := cnn_var.b_out;
  train_w_hidden := cnn_var.w_hidden;
  train_b_hidden := cnn_var.b_hidden;
  train_e := 0;
  while train_e < epochs do begin
  train_s := 0;
  while train_s < Length(samples) do begin
  train_data := samples[train_s].image;
  train_target := samples[train_s].target;
  train_maps := [];
  train_i := 0;
  while train_i < Length(cnn_var.conv_kernels) do begin
  train_conv_map := convolve(train_data, cnn_var.conv_kernels[train_i], cnn_var.conv_step, cnn_var.conv_bias[train_i]);
  train_pooled := average_pool(train_conv_map, cnn_var.pool_size);
  train_maps := concat(train_maps, [train_pooled]);
  train_i := train_i + 1;
end;
  train_flat := flatten(train_maps);
  train_hidden_net := vec_add(vec_mul_mat(train_flat, train_w_hidden), train_b_hidden);
  train_hidden_out := vec_map_sig(train_hidden_net);
  train_out_net := vec_add(vec_mul_mat(train_hidden_out, train_w_out), train_b_out);
  train_out_ := vec_map_sig(train_out_net);
  train_error_out := vec_sub(train_target, train_out_);
  train_pd_out := vec_mul(train_error_out, vec_mul(train_out_, vec_sub([1, 1], train_out_)));
  train_error_hidden := matT_vec_mul(train_w_out, train_pd_out);
  train_pd_hidden := vec_mul(train_error_hidden, vec_mul(train_hidden_out, vec_sub([1, 1], train_hidden_out)));
  train_j := 0;
  while train_j < Length(train_w_out) do begin
  train_k := 0;
  while train_k < Length(train_w_out[train_j]) do begin
  train_w_out[train_j][train_k] := train_w_out[train_j][train_k] + ((cnn_var.rate_weight * train_hidden_out[train_j]) * train_pd_out[train_k]);
  train_k := train_k + 1;
end;
  train_j := train_j + 1;
end;
  train_j := 0;
  while train_j < Length(train_b_out) do begin
  train_b_out[train_j] := train_b_out[train_j] - (cnn_var.rate_bias * train_pd_out[train_j]);
  train_j := train_j + 1;
end;
  train_i_h := 0;
  while train_i_h < Length(train_w_hidden) do begin
  train_j_h := 0;
  while train_j_h < Length(train_w_hidden[train_i_h]) do begin
  train_w_hidden[train_i_h][train_j_h] := train_w_hidden[train_i_h][train_j_h] + ((cnn_var.rate_weight * train_flat[train_i_h]) * train_pd_hidden[train_j_h]);
  train_j_h := train_j_h + 1;
end;
  train_i_h := train_i_h + 1;
end;
  train_j := 0;
  while train_j < Length(train_b_hidden) do begin
  train_b_hidden[train_j] := train_b_hidden[train_j] - (cnn_var.rate_bias * train_pd_hidden[train_j]);
  train_j := train_j + 1;
end;
  train_s := train_s + 1;
end;
  train_e := train_e + 1;
end;
  exit(makeCNN(cnn_var.conv_kernels, cnn_var.conv_bias, cnn_var.conv_step, cnn_var.pool_size, train_w_hidden, train_w_out, train_b_hidden, train_b_out, cnn_var.rate_weight, cnn_var.rate_bias));
end;
procedure main();
var
  main_cnn_var: CNN;
  main_image: array of array of real;
  main_sample: TrainSample;
  main_trained: CNN;
begin
  main_cnn_var := new_cnn();
  main_image := [[1, 0, 1, 0], [0, 1, 0, 1], [1, 0, 1, 0], [0, 1, 0, 1]];
  main_sample := makeTrainSample(main_image, [1, 0]);
  writeln('Before training:', ' ', list_real_to_str(forward(main_cnn_var, main_image)));
  main_trained := train(main_cnn_var, [main_sample], 50);
  writeln('After training:', ' ', list_real_to_str(forward(main_trained, main_image)));
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
end.
