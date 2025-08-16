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
  samples: array of array of real;
  labels: array of real;
  model: RealArrayArray;
  tol: real;
  x: RealArray;
  alphas: RealArray;
  max_passes: integer;
  c: real;
  a: real;
  b: real;
function dot(a: RealArray; b: RealArray): real; forward;
function maxf(a: real; b: real): real; forward;
function minf(a: real; b: real): real; forward;
function absf(x: real): real; forward;
function predict_raw(samples: RealArrayArray; labels: RealArray; alphas: RealArray; b: real; x: RealArray): real; forward;
function smo_train(samples: RealArrayArray; labels: RealArray; c: real; tol: real; max_passes: integer): RealArrayArray; forward;
function predict(samples: RealArrayArray; labels: RealArray; model: RealArrayArray; x: RealArray): real; forward;
function dot(a: RealArray; b: RealArray): real;
var
  dot_sum: real;
  dot_i: integer;
begin
  dot_sum := 0;
  dot_i := 0;
  while dot_i < Length(a) do begin
  dot_sum := dot_sum + (a[dot_i] * b[dot_i]);
  dot_i := dot_i + 1;
end;
  exit(dot_sum);
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
function absf(x: real): real;
begin
  if x >= 0 then begin
  exit(x);
end;
  exit(0 - x);
end;
function predict_raw(samples: RealArrayArray; labels: RealArray; alphas: RealArray; b: real; x: RealArray): real;
var
  predict_raw_res: real;
  predict_raw_i: integer;
begin
  predict_raw_res := 0;
  predict_raw_i := 0;
  while predict_raw_i < Length(samples) do begin
  predict_raw_res := predict_raw_res + ((alphas[predict_raw_i] * labels[predict_raw_i]) * dot(samples[predict_raw_i], x));
  predict_raw_i := predict_raw_i + 1;
end;
  exit(predict_raw_res + b);
end;
function smo_train(samples: RealArrayArray; labels: RealArray; c: real; tol: real; max_passes: integer): RealArrayArray;
var
  smo_train_m: integer;
  smo_train_alphas: array of real;
  smo_train_i: integer;
  smo_train_b: real;
  smo_train_passes: integer;
  smo_train_num_changed: integer;
  smo_train_i1: integer;
  smo_train_Ei: real;
  smo_train_i2: integer;
  smo_train_Ej: real;
  smo_train_alpha1_old: real;
  smo_train_alpha2_old: real;
  smo_train_L: real;
  smo_train_H: real;
  smo_train_eta: real;
  smo_train_b1: real;
  smo_train_b2: real;
begin
  smo_train_m := Length(samples);
  smo_train_alphas := [];
  smo_train_i := 0;
  while smo_train_i < smo_train_m do begin
  smo_train_alphas := concat(smo_train_alphas, [0]);
  smo_train_i := smo_train_i + 1;
end;
  smo_train_b := 0;
  smo_train_passes := 0;
  while smo_train_passes < max_passes do begin
  smo_train_num_changed := 0;
  smo_train_i1 := 0;
  while smo_train_i1 < smo_train_m do begin
  smo_train_Ei := predict_raw(samples, labels, smo_train_alphas, smo_train_b, samples[smo_train_i1]) - labels[smo_train_i1];
  if (((labels[smo_train_i1] * smo_train_Ei) < (0 - tol)) and (smo_train_alphas[smo_train_i1] < c)) or (((labels[smo_train_i1] * smo_train_Ei) > tol) and (smo_train_alphas[smo_train_i1] > 0)) then begin
  smo_train_i2 := (smo_train_i1 + 1) mod smo_train_m;
  smo_train_Ej := predict_raw(samples, labels, smo_train_alphas, smo_train_b, samples[smo_train_i2]) - labels[smo_train_i2];
  smo_train_alpha1_old := smo_train_alphas[smo_train_i1];
  smo_train_alpha2_old := smo_train_alphas[smo_train_i2];
  smo_train_L := 0;
  smo_train_H := 0;
  if labels[smo_train_i1] <> labels[smo_train_i2] then begin
  smo_train_L := maxf(0, smo_train_alpha2_old - smo_train_alpha1_old);
  smo_train_H := minf(c, (c + smo_train_alpha2_old) - smo_train_alpha1_old);
end else begin
  smo_train_L := maxf(0, (smo_train_alpha2_old + smo_train_alpha1_old) - c);
  smo_train_H := minf(c, smo_train_alpha2_old + smo_train_alpha1_old);
end;
  if smo_train_L = smo_train_H then begin
  smo_train_i1 := smo_train_i1 + 1;
  continue;
end;
  smo_train_eta := ((2 * dot(samples[smo_train_i1], samples[smo_train_i2])) - dot(samples[smo_train_i1], samples[smo_train_i1])) - dot(samples[smo_train_i2], samples[smo_train_i2]);
  if smo_train_eta >= 0 then begin
  smo_train_i1 := smo_train_i1 + 1;
  continue;
end;
  smo_train_alphas[smo_train_i2] := smo_train_alpha2_old - ((labels[smo_train_i2] * (smo_train_Ei - smo_train_Ej)) / smo_train_eta);
  if smo_train_alphas[smo_train_i2] > smo_train_H then begin
  smo_train_alphas[smo_train_i2] := smo_train_H;
end;
  if smo_train_alphas[smo_train_i2] < smo_train_L then begin
  smo_train_alphas[smo_train_i2] := smo_train_L;
end;
  if absf(smo_train_alphas[smo_train_i2] - smo_train_alpha2_old) < 1e-05 then begin
  smo_train_i1 := smo_train_i1 + 1;
  continue;
end;
  smo_train_alphas[smo_train_i1] := smo_train_alpha1_old + ((labels[smo_train_i1] * labels[smo_train_i2]) * (smo_train_alpha2_old - smo_train_alphas[smo_train_i2]));
  smo_train_b1 := ((smo_train_b - smo_train_Ei) - ((labels[smo_train_i1] * (smo_train_alphas[smo_train_i1] - smo_train_alpha1_old)) * dot(samples[smo_train_i1], samples[smo_train_i1]))) - ((labels[smo_train_i2] * (smo_train_alphas[smo_train_i2] - smo_train_alpha2_old)) * dot(samples[smo_train_i1], samples[smo_train_i2]));
  smo_train_b2 := ((smo_train_b - smo_train_Ej) - ((labels[smo_train_i1] * (smo_train_alphas[smo_train_i1] - smo_train_alpha1_old)) * dot(samples[smo_train_i1], samples[smo_train_i2]))) - ((labels[smo_train_i2] * (smo_train_alphas[smo_train_i2] - smo_train_alpha2_old)) * dot(samples[smo_train_i2], samples[smo_train_i2]));
  if (smo_train_alphas[smo_train_i1] > 0) and (smo_train_alphas[smo_train_i1] < c) then begin
  smo_train_b := smo_train_b1;
end else begin
  if (smo_train_alphas[smo_train_i2] > 0) and (smo_train_alphas[smo_train_i2] < c) then begin
  smo_train_b := smo_train_b2;
end else begin
  smo_train_b := (smo_train_b1 + smo_train_b2) / 2;
end;
end;
  smo_train_num_changed := smo_train_num_changed + 1;
end;
  smo_train_i1 := smo_train_i1 + 1;
end;
  if smo_train_num_changed = 0 then begin
  smo_train_passes := smo_train_passes + 1;
end else begin
  smo_train_passes := 0;
end;
end;
  exit([smo_train_alphas, [smo_train_b]]);
end;
function predict(samples: RealArrayArray; labels: RealArray; model: RealArrayArray; x: RealArray): real;
var
  predict_alphas: array of real;
  predict_b: real;
  predict_val: real;
begin
  predict_alphas := model[0];
  predict_b := model[1][0];
  predict_val := predict_raw(samples, labels, predict_alphas, predict_b, x);
  if predict_val >= 0 then begin
  exit(1);
end;
  exit(-1);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  samples := [[2, 2], [1.5, 1.5], [0, 0], [0.5, 0]];
  labels := [1, 1, -1, -1];
  model := smo_train(samples, labels, 1, 0.001, 10);
  writeln(predict(samples, labels, model, [1.5, 1]));
  writeln(predict(samples, labels, model, [0.2, 0.1]));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
