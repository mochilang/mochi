{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type SVC = record
  weights: array of real;
  bias: real;
  lr: real;
  lambda: real;
  epochs: integer;
end;
type RealArray = array of real;
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
  xs: array of RealArray;
  ys: array of integer;
  base: SVC;
  model: SVC;
  x: RealArray;
  a: RealArray;
  epochs: integer;
  b: RealArray;
  lambda: real;
  lr: real;
function makeSVC(weights: RealArray; bias: real; lr: real; lambda: real; epochs: integer): SVC; forward;
function dot(a: RealArray; b: RealArray): real; forward;
function new_svc(lr: real; lambda: real; epochs: integer): SVC; forward;
function fit(model: SVC; xs: RealArrayArray; ys: IntArray): SVC; forward;
function predict(model: SVC; x: RealArray): integer; forward;
function makeSVC(weights: RealArray; bias: real; lr: real; lambda: real; epochs: integer): SVC;
begin
  Result.weights := weights;
  Result.bias := bias;
  Result.lr := lr;
  Result.lambda := lambda;
  Result.epochs := epochs;
end;
function dot(a: RealArray; b: RealArray): real;
var
  dot_s: real;
  dot_i: integer;
begin
  dot_s := 0;
  dot_i := 0;
  while dot_i < Length(a) do begin
  dot_s := dot_s + (a[dot_i] * b[dot_i]);
  dot_i := dot_i + 1;
end;
  exit(dot_s);
end;
function new_svc(lr: real; lambda: real; epochs: integer): SVC;
begin
  exit(makeSVC([], 0, lr, lambda, epochs));
end;
function fit(model: SVC; xs: RealArrayArray; ys: IntArray): SVC;
var
  fit_n_features: integer;
  fit_w: array of real;
  fit_i: integer;
  fit_b: real;
  fit_epoch: integer;
  fit_j: integer;
  fit_x: array of real;
  fit_y: real;
  fit_prod: real;
  fit_k: integer;
  fit_k_16: integer;
begin
  fit_n_features := Length(xs[0]);
  fit_w := [];
  fit_i := 0;
  while fit_i < fit_n_features do begin
  fit_w := concat(fit_w, [0]);
  fit_i := fit_i + 1;
end;
  fit_b := 0;
  fit_epoch := 0;
  while fit_epoch < model.epochs do begin
  fit_j := 0;
  while fit_j < Length(xs) do begin
  fit_x := xs[fit_j];
  fit_y := Double(ys[fit_j]);
  fit_prod := dot(fit_w, fit_x) + fit_b;
  if (fit_y * fit_prod) < 1 then begin
  fit_k := 0;
  while fit_k < Length(fit_w) do begin
  fit_w[fit_k] := fit_w[fit_k] + (model.lr * ((fit_y * fit_x[fit_k]) - ((2 * model.lambda) * fit_w[fit_k])));
  fit_k := fit_k + 1;
end;
  fit_b := fit_b + (model.lr * fit_y);
end else begin
  fit_k_16 := 0;
  while fit_k_16 < Length(fit_w) do begin
  fit_w[fit_k_16] := fit_w[fit_k_16] - (model.lr * ((2 * model.lambda) * fit_w[fit_k_16]));
  fit_k_16 := fit_k_16 + 1;
end;
end;
  fit_j := fit_j + 1;
end;
  fit_epoch := fit_epoch + 1;
end;
  exit(makeSVC(fit_w, fit_b, model.lr, model.lambda, model.epochs));
end;
function predict(model: SVC; x: RealArray): integer;
var
  predict_s: real;
begin
  predict_s := dot(model.weights, x) + model.bias;
  if predict_s >= 0 then begin
  exit(1);
end else begin
  exit(-1);
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  xs := [[0, 1], [0, 2], [1, 1], [1, 2]];
  ys := [1, 1, -1, -1];
  base := new_svc(0.01, 0.01, 1000);
  model := fit(base, xs, ys);
  writeln(predict(model, [0, 1]));
  writeln(predict(model, [1, 1]));
  writeln(predict(model, [2, 2]));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
