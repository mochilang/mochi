{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type LSTMWeights = record
  w_i: real;
  u_i: real;
  b_i: real;
  w_f: real;
  u_f: real;
  b_f: real;
  w_o: real;
  u_o: real;
  b_o: real;
  w_c: real;
  u_c: real;
  b_c: real;
  w_y: real;
  b_y: real;
end;
type LSTMState = record
  i: array of real;
  f: array of real;
  o: array of real;
  g: array of real;
  c: array of real;
  h: array of real;
end;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
type Samples = record
  x: array of RealArray;
  y: array of real;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  data: array of real;
  look_back: integer;
  epochs: integer;
  lr: real;
  w: LSTMWeights;
  test_seq: array of real;
  pred: real;
  x: real;
  seq: RealArray;
  target: real;
  s: LSTMState;
function makeSamples(x: RealArrayArray; y: RealArray): Samples; forward;
function makeLSTMState(i: RealArray; f: RealArray; o: RealArray; g: RealArray; c: RealArray; h: RealArray): LSTMState; forward;
function makeLSTMWeights(w_i: real; u_i: real; b_i: real; w_f: real; u_f: real; b_f: real; w_o: real; u_o: real; b_o: real; w_c: real; u_c: real; b_c: real; w_y: real; b_y: real): LSTMWeights; forward;
function exp_approx(x: real): real; forward;
function sigmoid(x: real): real; forward;
function tanh_approx(x: real): real; forward;
function forward(seq: RealArray; w: LSTMWeights): LSTMState; forward;
function backward(seq: RealArray; target: real; w: LSTMWeights; s: LSTMState; lr: real): LSTMWeights; forward;
function make_samples(data: RealArray; look_back: integer): Samples; forward;
function init_weights(): LSTMWeights; forward;
function train(data: RealArray; look_back: integer; epochs: integer; lr: real): LSTMWeights; forward;
function predict(seq: RealArray; w: LSTMWeights): real; forward;
function makeSamples(x: RealArrayArray; y: RealArray): Samples;
begin
  Result.x := x;
  Result.y := y;
end;
function makeLSTMState(i: RealArray; f: RealArray; o: RealArray; g: RealArray; c: RealArray; h: RealArray): LSTMState;
begin
  Result.i := i;
  Result.f := f;
  Result.o := o;
  Result.g := g;
  Result.c := c;
  Result.h := h;
end;
function makeLSTMWeights(w_i: real; u_i: real; b_i: real; w_f: real; u_f: real; b_f: real; w_o: real; u_o: real; b_o: real; w_c: real; u_c: real; b_c: real; w_y: real; b_y: real): LSTMWeights;
begin
  Result.w_i := w_i;
  Result.u_i := u_i;
  Result.b_i := b_i;
  Result.w_f := w_f;
  Result.u_f := u_f;
  Result.b_f := b_f;
  Result.w_o := w_o;
  Result.u_o := u_o;
  Result.b_o := b_o;
  Result.w_c := w_c;
  Result.u_c := u_c;
  Result.b_c := b_c;
  Result.w_y := w_y;
  Result.b_y := b_y;
end;
function exp_approx(x: real): real;
var
  exp_approx_sum: real;
  exp_approx_term: real;
  exp_approx_n: integer;
begin
  exp_approx_sum := 1;
  exp_approx_term := 1;
  exp_approx_n := 1;
  while exp_approx_n < 20 do begin
  exp_approx_term := (exp_approx_term * x) / Double(exp_approx_n);
  exp_approx_sum := exp_approx_sum + exp_approx_term;
  exp_approx_n := exp_approx_n + 1;
end;
  exit(exp_approx_sum);
end;
function sigmoid(x: real): real;
begin
  exit(1 / (1 + exp_approx(-x)));
end;
function tanh_approx(x: real): real;
var
  tanh_approx_e: real;
begin
  tanh_approx_e := exp_approx(2 * x);
  exit((tanh_approx_e - 1) / (tanh_approx_e + 1));
end;
function forward(seq: RealArray; w: LSTMWeights): LSTMState;
var
  forward_i_arr: array of real;
  forward_f_arr: array of real;
  forward_o_arr: array of real;
  forward_g_arr: array of real;
  forward_c_arr: array of real;
  forward_h_arr: array of real;
  forward_t: integer;
  forward_x: real;
  forward_h_prev: real;
  forward_c_prev: real;
  forward_i_t: real;
  forward_f_t: real;
  forward_o_t: real;
  forward_g_t: real;
  forward_c_t: real;
  forward_h_t: real;
begin
  forward_i_arr := [];
  forward_f_arr := [];
  forward_o_arr := [];
  forward_g_arr := [];
  forward_c_arr := [0];
  forward_h_arr := [0];
  forward_t := 0;
  while forward_t < Length(seq) do begin
  forward_x := seq[forward_t];
  forward_h_prev := forward_h_arr[forward_t];
  forward_c_prev := forward_c_arr[forward_t];
  forward_i_t := sigmoid(((w.w_i * forward_x) + (w.u_i * forward_h_prev)) + w.b_i);
  forward_f_t := sigmoid(((w.w_f * forward_x) + (w.u_f * forward_h_prev)) + w.b_f);
  forward_o_t := sigmoid(((w.w_o * forward_x) + (w.u_o * forward_h_prev)) + w.b_o);
  forward_g_t := tanh_approx(((w.w_c * forward_x) + (w.u_c * forward_h_prev)) + w.b_c);
  forward_c_t := (forward_f_t * forward_c_prev) + (forward_i_t * forward_g_t);
  forward_h_t := forward_o_t * tanh_approx(forward_c_t);
  forward_i_arr := concat(forward_i_arr, [forward_i_t]);
  forward_f_arr := concat(forward_f_arr, [forward_f_t]);
  forward_o_arr := concat(forward_o_arr, [forward_o_t]);
  forward_g_arr := concat(forward_g_arr, [forward_g_t]);
  forward_c_arr := concat(forward_c_arr, [forward_c_t]);
  forward_h_arr := concat(forward_h_arr, [forward_h_t]);
  forward_t := forward_t + 1;
end;
  exit(makeLSTMState(forward_i_arr, forward_f_arr, forward_o_arr, forward_g_arr, forward_c_arr, forward_h_arr));
end;
function backward(seq: RealArray; target: real; w: LSTMWeights; s: LSTMState; lr: real): LSTMWeights;
var
  backward_dw_i: real;
  backward_du_i: real;
  backward_db_i: real;
  backward_dw_f: real;
  backward_du_f: real;
  backward_db_f: real;
  backward_dw_o: real;
  backward_du_o: real;
  backward_db_o: real;
  backward_dw_c: real;
  backward_du_c: real;
  backward_db_c: real;
  backward_dw_y: real;
  backward_db_y: real;
  backward_T: integer;
  backward_h_last: real;
  backward_y: real;
  backward_dy: real;
  backward_dh_next: real;
  backward_dc_next: real;
  backward_t_44: integer;
  backward_i_t: real;
  backward_f_t: real;
  backward_o_t: real;
  backward_g_t: real;
  backward_c_t: real;
  backward_c_prev: real;
  backward_h_prev: real;
  backward_tanh_c: real;
  backward_do_t: real;
  backward_da_o: real;
  backward_dc: real;
  backward_di_t: real;
  backward_da_i: real;
  backward_dg_t: real;
  backward_da_g: real;
  backward_df_t: real;
  backward_da_f: real;
begin
  backward_dw_i := 0;
  backward_du_i := 0;
  backward_db_i := 0;
  backward_dw_f := 0;
  backward_du_f := 0;
  backward_db_f := 0;
  backward_dw_o := 0;
  backward_du_o := 0;
  backward_db_o := 0;
  backward_dw_c := 0;
  backward_du_c := 0;
  backward_db_c := 0;
  backward_dw_y := 0;
  backward_db_y := 0;
  backward_T := Length(seq);
  backward_h_last := s.h[backward_T];
  backward_y := (w.w_y * backward_h_last) + w.b_y;
  backward_dy := backward_y - target;
  backward_dw_y := backward_dy * backward_h_last;
  backward_db_y := backward_dy;
  backward_dh_next := backward_dy * w.w_y;
  backward_dc_next := 0;
  backward_t_44 := backward_T - 1;
  while backward_t_44 >= 0 do begin
  backward_i_t := s.i[backward_t_44];
  backward_f_t := s.f[backward_t_44];
  backward_o_t := s.o[backward_t_44];
  backward_g_t := s.g[backward_t_44];
  backward_c_t := s.c[backward_t_44 + 1];
  backward_c_prev := s.c[backward_t_44];
  backward_h_prev := s.h[backward_t_44];
  backward_tanh_c := tanh_approx(backward_c_t);
  backward_do_t := backward_dh_next * backward_tanh_c;
  backward_da_o := (backward_do_t * backward_o_t) * (1 - backward_o_t);
  backward_dc := ((backward_dh_next * backward_o_t) * (1 - (backward_tanh_c * backward_tanh_c))) + backward_dc_next;
  backward_di_t := backward_dc * backward_g_t;
  backward_da_i := (backward_di_t * backward_i_t) * (1 - backward_i_t);
  backward_dg_t := backward_dc * backward_i_t;
  backward_da_g := backward_dg_t * (1 - (backward_g_t * backward_g_t));
  backward_df_t := backward_dc * backward_c_prev;
  backward_da_f := (backward_df_t * backward_f_t) * (1 - backward_f_t);
  backward_dw_i := backward_dw_i + (backward_da_i * seq[backward_t_44]);
  backward_du_i := backward_du_i + (backward_da_i * backward_h_prev);
  backward_db_i := backward_db_i + backward_da_i;
  backward_dw_f := backward_dw_f + (backward_da_f * seq[backward_t_44]);
  backward_du_f := backward_du_f + (backward_da_f * backward_h_prev);
  backward_db_f := backward_db_f + backward_da_f;
  backward_dw_o := backward_dw_o + (backward_da_o * seq[backward_t_44]);
  backward_du_o := backward_du_o + (backward_da_o * backward_h_prev);
  backward_db_o := backward_db_o + backward_da_o;
  backward_dw_c := backward_dw_c + (backward_da_g * seq[backward_t_44]);
  backward_du_c := backward_du_c + (backward_da_g * backward_h_prev);
  backward_db_c := backward_db_c + backward_da_g;
  backward_dh_next := (((backward_da_i * w.u_i) + (backward_da_f * w.u_f)) + (backward_da_o * w.u_o)) + (backward_da_g * w.u_c);
  backward_dc_next := backward_dc * backward_f_t;
  backward_t_44 := backward_t_44 - 1;
end;
  w.w_y := w.w_y - (lr * backward_dw_y);
  w.b_y := w.b_y - (lr * backward_db_y);
  w.w_i := w.w_i - (lr * backward_dw_i);
  w.u_i := w.u_i - (lr * backward_du_i);
  w.b_i := w.b_i - (lr * backward_db_i);
  w.w_f := w.w_f - (lr * backward_dw_f);
  w.u_f := w.u_f - (lr * backward_du_f);
  w.b_f := w.b_f - (lr * backward_db_f);
  w.w_o := w.w_o - (lr * backward_dw_o);
  w.u_o := w.u_o - (lr * backward_du_o);
  w.b_o := w.b_o - (lr * backward_db_o);
  w.w_c := w.w_c - (lr * backward_dw_c);
  w.u_c := w.u_c - (lr * backward_du_c);
  w.b_c := w.b_c - (lr * backward_db_c);
  exit(w);
end;
function make_samples(data: RealArray; look_back: integer): Samples;
var
  make_samples_X: array of RealArray;
  make_samples_Y: array of real;
  make_samples_i: integer;
  make_samples_seq: array of integer;
begin
  make_samples_X := [];
  make_samples_Y := [];
  make_samples_i := 0;
  while (make_samples_i + look_back) < Length(data) do begin
  make_samples_seq := copy(data, make_samples_i, (make_samples_i + look_back - (make_samples_i)));
  make_samples_X := concat(make_samples_X, [make_samples_seq]);
  make_samples_Y := concat(make_samples_Y, [data[make_samples_i + look_back]]);
  make_samples_i := make_samples_i + 1;
end;
  exit(makeSamples(make_samples_X, make_samples_Y));
end;
function init_weights(): LSTMWeights;
begin
  exit(makeLSTMWeights(0.1, 0.2, 0, 0.1, 0.2, 0, 0.1, 0.2, 0, 0.1, 0.2, 0, 0.1, 0));
end;
function train(data: RealArray; look_back: integer; epochs: integer; lr: real): LSTMWeights;
var
  train_samples_var: Samples;
  train_w: LSTMWeights;
  train_ep: integer;
  train_j: integer;
  train_seq: array of real;
  train_target: real;
  train_state: LSTMState;
begin
  train_samples_var := make_samples(data, look_back);
  train_w := init_weights();
  train_ep := 0;
  while train_ep < epochs do begin
  train_j := 0;
  while train_j < Length(train_samples_var.x) do begin
  train_seq := train_samples_var.x[train_j];
  train_target := train_samples_var.y[train_j];
  train_state := forward(train_seq, train_w);
  train_w := backward(train_seq, train_target, train_w, train_state, lr);
  train_j := train_j + 1;
end;
  train_ep := train_ep + 1;
end;
  exit(train_w);
end;
function predict(seq: RealArray; w: LSTMWeights): real;
var
  predict_state: LSTMState;
  predict_h_last: real;
begin
  predict_state := forward(seq, w);
  predict_h_last := predict_state.h[Length(predict_state.h) - 1];
  exit((w.w_y * predict_h_last) + w.b_y);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  data := [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8];
  look_back := 3;
  epochs := 200;
  lr := 0.1;
  w := train(data, look_back, epochs, lr);
  test_seq := [0.6, 0.7, 0.8];
  pred := predict(test_seq, w);
  writeln('Predicted value: ' + FloatToStr(pred));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
