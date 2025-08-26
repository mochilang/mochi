{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
type RealArrayArrayArray = array of RealArrayArray;
type CNN = record
  conv_kernels: array of RealArrayArray;
  conv_bias: array of real;
  conv_step: int64;
  pool_size: int64;
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
  seed: int64;
function makeTrainSample(image: RealArrayArray; target: RealArray): TrainSample; forward;
function makeCNN(conv_kernels: RealArrayArrayArray; conv_bias: RealArray; conv_step: int64; pool_size: int64; w_hidden: RealArrayArray; w_out: RealArrayArray; b_hidden: RealArray; b_out: RealArray; rate_weight: real; rate_bias: real): CNN; forward;
function random(): real; forward;
function sigmoid(sigmoid_x: real): real; forward;
function to_float(to_float_x: int64): real; forward;
function exp_(exp__x: real): real; forward;
function convolve(convolve_data: RealArrayArray; convolve_kernel: RealArrayArray; convolve_step: int64; convolve_bias: real): RealArrayArray; forward;
function average_pool(average_pool_map: RealArrayArray; average_pool_size: int64): RealArrayArray; forward;
function flatten(flatten_maps: RealArrayArrayArray): RealArray; forward;
function vec_mul_mat(vec_mul_mat_v: RealArray; vec_mul_mat_m: RealArrayArray): RealArray; forward;
function matT_vec_mul(matT_vec_mul_m: RealArrayArray; matT_vec_mul_v: RealArray): RealArray; forward;
function vec_add(vec_add_a: RealArray; vec_add_b: RealArray): RealArray; forward;
function vec_sub(vec_sub_a: RealArray; vec_sub_b: RealArray): RealArray; forward;
function vec_mul(vec_mul_a: RealArray; vec_mul_b: RealArray): RealArray; forward;
function vec_map_sig(vec_map_sig_v: RealArray): RealArray; forward;
function new_cnn(): CNN; forward;
function forward(forward_cnn_var: CNN; forward_data: RealArrayArray): RealArray; forward;
function train(train_cnn_var: CNN; train_samples: TrainSampleArray; train_epochs: int64): CNN; forward;
procedure main(); forward;
function makeTrainSample(image: RealArrayArray; target: RealArray): TrainSample;
begin
  Result.image := image;
  Result.target := target;
end;
function makeCNN(conv_kernels: RealArrayArrayArray; conv_bias: RealArray; conv_step: int64; pool_size: int64; w_hidden: RealArrayArray; w_out: RealArrayArray; b_hidden: RealArray; b_out: RealArray; rate_weight: real; rate_bias: real): CNN;
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
function sigmoid(sigmoid_x: real): real;
begin
  exit(1 / (1 + exp(-sigmoid_x)));
end;
function to_float(to_float_x: int64): real;
begin
  exit(to_float_x * 1);
end;
function exp_(exp__x: real): real;
var
  exp__term: real;
  exp__sum: real;
  exp__n: int64;
begin
  exp__term := 1;
  exp__sum := 1;
  exp__n := 1;
  while exp__n < 20 do begin
  exp__term := (exp__term * exp__x) / to_float(exp__n);
  exp__sum := exp__sum + exp__term;
  exp__n := exp__n + 1;
end;
  exit(exp__sum);
end;
function convolve(convolve_data: RealArrayArray; convolve_kernel: RealArrayArray; convolve_step: int64; convolve_bias: real): RealArrayArray;
var
  convolve_size_data: integer;
  convolve_size_kernel: integer;
  convolve_out_: array of RealArray;
  convolve_i: int64;
  convolve_row: array of real;
  convolve_j: int64;
  convolve_sum: real;
  convolve_a: int64;
  convolve_b: int64;
begin
  convolve_size_data := Length(convolve_data);
  convolve_size_kernel := Length(convolve_kernel);
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
  convolve_sum := convolve_sum + (convolve_data[convolve_i + convolve_a][convolve_j + convolve_b] * convolve_kernel[convolve_a][convolve_b]);
  convolve_b := convolve_b + 1;
end;
  convolve_a := convolve_a + 1;
end;
  convolve_row := concat(convolve_row, [sigmoid(convolve_sum - convolve_bias)]);
  convolve_j := convolve_j + convolve_step;
end;
  convolve_out_ := concat(convolve_out_, [convolve_row]);
  convolve_i := convolve_i + convolve_step;
end;
  exit(convolve_out_);
end;
function average_pool(average_pool_map: RealArrayArray; average_pool_size: int64): RealArrayArray;
var
  average_pool_out_: array of RealArray;
  average_pool_i: int64;
  average_pool_row: array of real;
  average_pool_j: int64;
  average_pool_sum: real;
  average_pool_a: int64;
  average_pool_b: int64;
begin
  average_pool_out_ := [];
  average_pool_i := 0;
  while average_pool_i < Length(average_pool_map) do begin
  average_pool_row := [];
  average_pool_j := 0;
  while average_pool_j < Length(average_pool_map[average_pool_i]) do begin
  average_pool_sum := 0;
  average_pool_a := 0;
  while average_pool_a < average_pool_size do begin
  average_pool_b := 0;
  while average_pool_b < average_pool_size do begin
  average_pool_sum := average_pool_sum + average_pool_map[average_pool_i + average_pool_a][average_pool_j + average_pool_b];
  average_pool_b := average_pool_b + 1;
end;
  average_pool_a := average_pool_a + 1;
end;
  average_pool_row := concat(average_pool_row, [average_pool_sum / Double(average_pool_size * average_pool_size)]);
  average_pool_j := average_pool_j + average_pool_size;
end;
  average_pool_out_ := concat(average_pool_out_, [average_pool_row]);
  average_pool_i := average_pool_i + average_pool_size;
end;
  exit(average_pool_out_);
end;
function flatten(flatten_maps: RealArrayArrayArray): RealArray;
var
  flatten_out_: array of real;
  flatten_i: int64;
  flatten_j: int64;
  flatten_k: int64;
begin
  flatten_out_ := [];
  flatten_i := 0;
  while flatten_i < Length(flatten_maps) do begin
  flatten_j := 0;
  while flatten_j < Length(flatten_maps[flatten_i]) do begin
  flatten_k := 0;
  while flatten_k < Length(flatten_maps[flatten_i][flatten_j]) do begin
  flatten_out_ := concat(flatten_out_, [flatten_maps[flatten_i][flatten_j][flatten_k]]);
  flatten_k := flatten_k + 1;
end;
  flatten_j := flatten_j + 1;
end;
  flatten_i := flatten_i + 1;
end;
  exit(flatten_out_);
end;
function vec_mul_mat(vec_mul_mat_v: RealArray; vec_mul_mat_m: RealArrayArray): RealArray;
var
  vec_mul_mat_cols: int64;
  vec_mul_mat_res: array of real;
  vec_mul_mat_j: int64;
  vec_mul_mat_sum: real;
  vec_mul_mat_i: int64;
begin
  vec_mul_mat_cols := Length(vec_mul_mat_m[0]);
  vec_mul_mat_res := [];
  vec_mul_mat_j := 0;
  while vec_mul_mat_j < vec_mul_mat_cols do begin
  vec_mul_mat_sum := 0;
  vec_mul_mat_i := 0;
  while vec_mul_mat_i < Length(vec_mul_mat_v) do begin
  vec_mul_mat_sum := vec_mul_mat_sum + (vec_mul_mat_v[vec_mul_mat_i] * vec_mul_mat_m[vec_mul_mat_i][vec_mul_mat_j]);
  vec_mul_mat_i := vec_mul_mat_i + 1;
end;
  vec_mul_mat_res := concat(vec_mul_mat_res, [vec_mul_mat_sum]);
  vec_mul_mat_j := vec_mul_mat_j + 1;
end;
  exit(vec_mul_mat_res);
end;
function matT_vec_mul(matT_vec_mul_m: RealArrayArray; matT_vec_mul_v: RealArray): RealArray;
var
  matT_vec_mul_res: array of real;
  matT_vec_mul_i: int64;
  matT_vec_mul_sum: real;
  matT_vec_mul_j: int64;
begin
  matT_vec_mul_res := [];
  matT_vec_mul_i := 0;
  while matT_vec_mul_i < Length(matT_vec_mul_m) do begin
  matT_vec_mul_sum := 0;
  matT_vec_mul_j := 0;
  while matT_vec_mul_j < Length(matT_vec_mul_m[matT_vec_mul_i]) do begin
  matT_vec_mul_sum := matT_vec_mul_sum + (matT_vec_mul_m[matT_vec_mul_i][matT_vec_mul_j] * matT_vec_mul_v[matT_vec_mul_j]);
  matT_vec_mul_j := matT_vec_mul_j + 1;
end;
  matT_vec_mul_res := concat(matT_vec_mul_res, [matT_vec_mul_sum]);
  matT_vec_mul_i := matT_vec_mul_i + 1;
end;
  exit(matT_vec_mul_res);
end;
function vec_add(vec_add_a: RealArray; vec_add_b: RealArray): RealArray;
var
  vec_add_res: array of real;
  vec_add_i: int64;
begin
  vec_add_res := [];
  vec_add_i := 0;
  while vec_add_i < Length(vec_add_a) do begin
  vec_add_res := concat(vec_add_res, [vec_add_a[vec_add_i] + vec_add_b[vec_add_i]]);
  vec_add_i := vec_add_i + 1;
end;
  exit(vec_add_res);
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
function vec_map_sig(vec_map_sig_v: RealArray): RealArray;
var
  vec_map_sig_res: array of real;
  vec_map_sig_i: int64;
begin
  vec_map_sig_res := [];
  vec_map_sig_i := 0;
  while vec_map_sig_i < Length(vec_map_sig_v) do begin
  vec_map_sig_res := concat(vec_map_sig_res, [sigmoid(vec_map_sig_v[vec_map_sig_i])]);
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
  new_cnn_conv_step: int64;
  new_cnn_pool_size: int64;
  new_cnn_input_size: int64;
  new_cnn_hidden_size: int64;
  new_cnn_output_size: int64;
  new_cnn_w_hidden: array of RealArray;
  new_cnn_i: int64;
  new_cnn_row: array of real;
  new_cnn_j: int64;
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
function forward(forward_cnn_var: CNN; forward_data: RealArrayArray): RealArray;
var
  forward_maps: array of RealArrayArray;
  forward_i: int64;
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
  while forward_i < Length(forward_cnn_var.conv_kernels) do begin
  forward_conv_map := convolve(forward_data, forward_cnn_var.conv_kernels[forward_i], forward_cnn_var.conv_step, forward_cnn_var.conv_bias[forward_i]);
  forward_pooled := average_pool(forward_conv_map, forward_cnn_var.pool_size);
  forward_maps := concat(forward_maps, [forward_pooled]);
  forward_i := forward_i + 1;
end;
  forward_flat := flatten(forward_maps);
  forward_hidden_net := vec_add(vec_mul_mat(forward_flat, forward_cnn_var.w_hidden), forward_cnn_var.b_hidden);
  forward_hidden_out := vec_map_sig(forward_hidden_net);
  forward_out_net := vec_add(vec_mul_mat(forward_hidden_out, forward_cnn_var.w_out), forward_cnn_var.b_out);
  forward_out_ := vec_map_sig(forward_out_net);
  exit(forward_out_);
end;
function train(train_cnn_var: CNN; train_samples: TrainSampleArray; train_epochs: int64): CNN;
var
  train_w_out: array of RealArray;
  train_b_out: array of real;
  train_w_hidden: array of RealArray;
  train_b_hidden: array of real;
  train_e: int64;
  train_s: int64;
  train_data: array of RealArray;
  train_target: array of real;
  train_maps: array of RealArrayArray;
  train_i: int64;
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
  train_j: int64;
  train_k: int64;
  train_i_h: int64;
  train_j_h: int64;
begin
  train_w_out := train_cnn_var.w_out;
  train_b_out := train_cnn_var.b_out;
  train_w_hidden := train_cnn_var.w_hidden;
  train_b_hidden := train_cnn_var.b_hidden;
  train_e := 0;
  while train_e < train_epochs do begin
  train_s := 0;
  while train_s < Length(train_samples) do begin
  train_data := train_samples[train_s].image;
  train_target := train_samples[train_s].target;
  train_maps := [];
  train_i := 0;
  while train_i < Length(train_cnn_var.conv_kernels) do begin
  train_conv_map := convolve(train_data, train_cnn_var.conv_kernels[train_i], train_cnn_var.conv_step, train_cnn_var.conv_bias[train_i]);
  train_pooled := average_pool(train_conv_map, train_cnn_var.pool_size);
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
  train_w_out[train_j][train_k] := train_w_out[train_j][train_k] + ((train_cnn_var.rate_weight * train_hidden_out[train_j]) * train_pd_out[train_k]);
  train_k := train_k + 1;
end;
  train_j := train_j + 1;
end;
  train_j := 0;
  while train_j < Length(train_b_out) do begin
  train_b_out[train_j] := train_b_out[train_j] - (train_cnn_var.rate_bias * train_pd_out[train_j]);
  train_j := train_j + 1;
end;
  train_i_h := 0;
  while train_i_h < Length(train_w_hidden) do begin
  train_j_h := 0;
  while train_j_h < Length(train_w_hidden[train_i_h]) do begin
  train_w_hidden[train_i_h][train_j_h] := train_w_hidden[train_i_h][train_j_h] + ((train_cnn_var.rate_weight * train_flat[train_i_h]) * train_pd_hidden[train_j_h]);
  train_j_h := train_j_h + 1;
end;
  train_i_h := train_i_h + 1;
end;
  train_j := 0;
  while train_j < Length(train_b_hidden) do begin
  train_b_hidden[train_j] := train_b_hidden[train_j] - (train_cnn_var.rate_bias * train_pd_hidden[train_j]);
  train_j := train_j + 1;
end;
  train_s := train_s + 1;
end;
  train_e := train_e + 1;
end;
  exit(makeCNN(train_cnn_var.conv_kernels, train_cnn_var.conv_bias, train_cnn_var.conv_step, train_cnn_var.pool_size, train_w_hidden, train_w_out, train_b_hidden, train_b_out, train_cnn_var.rate_weight, train_cnn_var.rate_bias));
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
  writeln('');
end.
