{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type PointLabel = record
  point: array of real;
  label_: integer;
end;
type KNN = record
  data: array of PointLabel;
  labels: array of string;
end;
type DistLabel = record
  dist: real;
  label_: integer;
end;
type RealArray = array of real;
type StrArray = array of string;
type IntArray = array of integer;
type DistLabelArray = array of DistLabel;
type RealArrayArray = array of RealArray;
type PointLabelArray = array of PointLabel;
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
  train_X: array of array of real;
  train_y: array of integer;
  classes: array of string;
  knn_var: KNN;
  point: array of real;
  train_target: IntArray;
  k: integer;
  pred_point: RealArray;
  x: real;
  train_data: RealArrayArray;
  b: RealArray;
  class_labels: StrArray;
  a: RealArray;
function makeDistLabel(dist: real; label_: integer): DistLabel; forward;
function makeKNN(data: PointLabelArray; labels: StrArray): KNN; forward;
function makePointLabel(point: RealArray; label_: integer): PointLabel; forward;
function sqrtApprox(x: real): real; forward;
function make_knn(train_data: RealArrayArray; train_target: IntArray; class_labels: StrArray): KNN; forward;
function euclidean_distance(a: RealArray; b: RealArray): real; forward;
function classify(classify_knn_var: KNN; pred_point: RealArray; k: integer): string; forward;
function makeDistLabel(dist: real; label_: integer): DistLabel;
begin
  Result.dist := dist;
  Result.label_ := label_;
end;
function makeKNN(data: PointLabelArray; labels: StrArray): KNN;
begin
  Result.data := data;
  Result.labels := labels;
end;
function makePointLabel(point: RealArray; label_: integer): PointLabel;
begin
  Result.point := point;
  Result.label_ := label_;
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrtApprox_guess := x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function make_knn(train_data: RealArrayArray; train_target: IntArray; class_labels: StrArray): KNN;
var
  make_knn_items: array of PointLabel;
  make_knn_i: integer;
  make_knn_pl: PointLabel;
begin
  make_knn_items := [];
  make_knn_i := 0;
  while make_knn_i < Length(train_data) do begin
  make_knn_pl := makePointLabel(train_data[make_knn_i], train_target[make_knn_i]);
  make_knn_items := concat(make_knn_items, [make_knn_pl]);
  make_knn_i := make_knn_i + 1;
end;
  exit(makeKNN(make_knn_items, class_labels));
end;
function euclidean_distance(a: RealArray; b: RealArray): real;
var
  euclidean_distance_sum: real;
  euclidean_distance_i: integer;
  euclidean_distance_diff: real;
begin
  euclidean_distance_sum := 0;
  euclidean_distance_i := 0;
  while euclidean_distance_i < Length(a) do begin
  euclidean_distance_diff := a[euclidean_distance_i] - b[euclidean_distance_i];
  euclidean_distance_sum := euclidean_distance_sum + (euclidean_distance_diff * euclidean_distance_diff);
  euclidean_distance_i := euclidean_distance_i + 1;
end;
  exit(sqrtApprox(euclidean_distance_sum));
end;
function classify(classify_knn_var: KNN; pred_point: RealArray; k: integer): string;
var
  classify_distances: array of DistLabel;
  classify_i: integer;
  classify_d: real;
  classify_votes: array of integer;
  classify_count: integer;
  classify_min_index: integer;
  classify_j: integer;
  classify_tally: array of integer;
  classify_t: integer;
  classify_v: integer;
  classify_lbl: integer;
  classify_max_idx: integer;
  classify_m: integer;
begin
  classify_distances := [];
  classify_i := 0;
  while classify_i < Length(knn_var.data) do begin
  classify_d := euclidean_distance(knn_var.data[classify_i].point, pred_point);
  classify_distances := concat(classify_distances, [makeDistLabel(classify_d, knn_var.data[classify_i].label_)]);
  classify_i := classify_i + 1;
end;
  classify_votes := [];
  classify_count := 0;
  while classify_count < k do begin
  classify_min_index := 0;
  classify_j := 1;
  while classify_j < Length(classify_distances) do begin
  if classify_distances[classify_j].dist < classify_distances[classify_min_index].dist then begin
  classify_min_index := classify_j;
end;
  classify_j := classify_j + 1;
end;
  classify_votes := concat(classify_votes, IntArray([classify_distances[classify_min_index].label_]));
  classify_distances[classify_min_index] := 1e+18;
  classify_count := classify_count + 1;
end;
  classify_tally := [];
  classify_t := 0;
  while classify_t < Length(knn_var.labels) do begin
  classify_tally := concat(classify_tally, IntArray([0]));
  classify_t := classify_t + 1;
end;
  classify_v := 0;
  while classify_v < Length(classify_votes) do begin
  classify_lbl := classify_votes[classify_v];
  classify_tally[classify_lbl] := classify_tally[classify_lbl] + 1;
  classify_v := classify_v + 1;
end;
  classify_max_idx := 0;
  classify_m := 1;
  while classify_m < Length(classify_tally) do begin
  if classify_tally[classify_m] > classify_tally[classify_max_idx] then begin
  classify_max_idx := classify_m;
end;
  classify_m := classify_m + 1;
end;
  exit(knn_var.labels[classify_max_idx]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  train_X := [[0, 0], [1, 0], [0, 1], [0.5, 0.5], [3, 3], [2, 3], [3, 2]];
  train_y := [0, 0, 0, 0, 1, 1, 1];
  classes := ['A', 'B'];
  knn_var := make_knn(train_X, train_y, classes);
  point := [1.2, 1.2];
  writeln(classify(knn_var, point, 5));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
