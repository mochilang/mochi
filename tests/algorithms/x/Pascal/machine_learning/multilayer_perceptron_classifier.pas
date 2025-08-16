{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  X: array of RealArray;
  Y: array of real;
  test_data: array of RealArray;
  w1: array of RealArray;
  b1: array of real;
  w2: array of real;
  b2: real;
  preds: IntArray;
  samples: RealArrayArray;
  epochs: integer;
  x: real;
  lr: real;
  y_39: IntArray;
function exp_taylor(x: real): real; forward;
function sigmoid(x: real): real; forward;
procedure train(epochs: integer; lr: real); forward;
function predict(samples: RealArrayArray): IntArray; forward;
function wrapper(wrapper_y_39: IntArray): IntArray; forward;
function exp_taylor(x: real): real;
var
  exp_taylor_term: real;
  exp_taylor_sum: real;
  exp_taylor_i: real;
begin
  exp_taylor_term := 1;
  exp_taylor_sum := 1;
  exp_taylor_i := 1;
  while exp_taylor_i < 20 do begin
  exp_taylor_term := (exp_taylor_term * x) / exp_taylor_i;
  exp_taylor_sum := exp_taylor_sum + exp_taylor_term;
  exp_taylor_i := exp_taylor_i + 1;
end;
  exit(exp_taylor_sum);
end;
function sigmoid(x: real): real;
begin
  exit(1 / (1 + exp_taylor(-x)));
end;
procedure train(epochs: integer; lr: real);
var
  train_e: integer;
  train_i: integer;
  train_x0: real;
  train_x1: real;
  train_target: real;
  train_z1: real;
  train_z2: real;
  train_h1: real;
  train_h2: real;
  train_z3: real;
  train_out_: real;
  train_error: real;
  train_d1: real;
  train_d2: real;
begin
  train_e := 0;
  while train_e < epochs do begin
  train_i := 0;
  while train_i < Length(X) do begin
  train_x0 := X[train_i][0];
  train_x1 := X[train_i][1];
  train_target := Y[train_i];
  train_z1 := ((w1[0][0] * train_x0) + (w1[1][0] * train_x1)) + b1[0];
  train_z2 := ((w1[0][1] * train_x0) + (w1[1][1] * train_x1)) + b1[1];
  train_h1 := sigmoid(train_z1);
  train_h2 := sigmoid(train_z2);
  train_z3 := ((w2[0] * train_h1) + (w2[1] * train_h2)) + b2;
  train_out_ := sigmoid(train_z3);
  train_error := train_out_ - train_target;
  train_d1 := ((train_h1 * (1 - train_h1)) * w2[0]) * train_error;
  train_d2 := ((train_h2 * (1 - train_h2)) * w2[1]) * train_error;
  w2[0] := w2[0] - ((lr * train_error) * train_h1);
  w2[1] := w2[1] - ((lr * train_error) * train_h2);
  b2 := b2 - (lr * train_error);
  w1[0][0] := w1[0][0] - ((lr * train_d1) * train_x0);
  w1[1][0] := w1[1][0] - ((lr * train_d1) * train_x1);
  b1[0] := b1[0] - (lr * train_d1);
  w1[0][1] := w1[0][1] - ((lr * train_d2) * train_x0);
  w1[1][1] := w1[1][1] - ((lr * train_d2) * train_x1);
  b1[1] := b1[1] - (lr * train_d2);
  train_i := train_i + 1;
end;
  train_e := train_e + 1;
end;
end;
function predict(samples: RealArrayArray): IntArray;
var
  predict_preds: array of integer;
  predict_i: integer;
  predict_x0: real;
  predict_x1: real;
  predict_z1: real;
  predict_z2: real;
  predict_h1: real;
  predict_h2: real;
  predict_z3: real;
  predict_out_: real;
  predict_label_: integer;
begin
  predict_preds := [];
  predict_i := 0;
  while predict_i < Length(samples) do begin
  predict_x0 := samples[predict_i][0];
  predict_x1 := samples[predict_i][1];
  predict_z1 := ((w1[0][0] * predict_x0) + (w1[1][0] * predict_x1)) + b1[0];
  predict_z2 := ((w1[0][1] * predict_x0) + (w1[1][1] * predict_x1)) + b1[1];
  predict_h1 := sigmoid(predict_z1);
  predict_h2 := sigmoid(predict_z2);
  predict_z3 := ((w2[0] * predict_h1) + (w2[1] * predict_h2)) + b2;
  predict_out_ := sigmoid(predict_z3);
  predict_label_ := 0;
  if predict_out_ >= 0.5 then begin
  predict_label_ := 1;
end;
  predict_preds := concat(predict_preds, IntArray([predict_label_]));
  predict_i := predict_i + 1;
end;
  exit(predict_preds);
end;
function wrapper(wrapper_y_39: IntArray): IntArray;
begin
  exit(y_39);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  X := [[0, 0], [1, 1], [1, 0], [0, 1]];
  Y := [0, 1, 0, 0];
  test_data := [[0, 0], [0, 1], [1, 1]];
  w1 := [[0.5, -0.5], [0.5, 0.5]];
  b1 := [0, 0];
  w2 := [0.5, -0.5];
  b2 := 0;
  train(4000, 0.5);
  preds := wrapper(predict(test_data));
  writeln(list_int_to_str(preds));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
