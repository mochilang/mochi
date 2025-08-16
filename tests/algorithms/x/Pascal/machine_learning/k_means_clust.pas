{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
type RealArrayArray = array of RealArray;
type KMeansResult = record
  centroids: array of RealArray;
  assignments: array of integer;
  heterogeneity: array of real;
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
function list_list_real_to_str(xs: array of RealArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_real_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  data: array of RealArray;
  k: integer;
  initial_centroids: array of RealArray;
  result_: KMeansResult;
  centroids: RealArrayArray;
  a: IntArray;
  assignment: IntArray;
  b: IntArray;
  max_iter: integer;
function makeKMeansResult(centroids: RealArrayArray; assignments: IntArray; heterogeneity: RealArray): KMeansResult; forward;
function distance_sq(a: RealArray; b: RealArray): real; forward;
function assign_clusters(data: RealArrayArray; centroids: RealArrayArray): IntArray; forward;
function revise_centroids(data: RealArrayArray; k: integer; assignment: IntArray): RealArrayArray; forward;
function compute_heterogeneity(data: RealArrayArray; centroids: RealArrayArray; assignment: IntArray): real; forward;
function lists_equal(a: IntArray; b: IntArray): boolean; forward;
function kmeans(data: RealArrayArray; k: integer; initial_centroids: RealArrayArray; max_iter: integer): KMeansResult; forward;
function makeKMeansResult(centroids: RealArrayArray; assignments: IntArray; heterogeneity: RealArray): KMeansResult;
begin
  Result.centroids := centroids;
  Result.assignments := assignments;
  Result.heterogeneity := heterogeneity;
end;
function distance_sq(a: RealArray; b: RealArray): real;
var
  distance_sq_sum: real;
  distance_sq_i: int64;
  distance_sq_diff: real;
begin
  distance_sq_sum := 0;
  for distance_sq_i := 0 to (Length(a) - 1) do begin
  distance_sq_diff := a[distance_sq_i] - b[distance_sq_i];
  distance_sq_sum := distance_sq_sum + (distance_sq_diff * distance_sq_diff);
end;
  exit(distance_sq_sum);
end;
function assign_clusters(data: RealArrayArray; centroids: RealArrayArray): IntArray;
var
  assign_clusters_assignments: array of integer;
  assign_clusters_i: int64;
  assign_clusters_best_idx: integer;
  assign_clusters_best: real;
  assign_clusters_j: int64;
  assign_clusters_dist: real;
begin
  assign_clusters_assignments := [];
  for assign_clusters_i := 0 to (Length(data) - 1) do begin
  assign_clusters_best_idx := 0;
  assign_clusters_best := distance_sq(data[assign_clusters_i], centroids[0]);
  for assign_clusters_j := 1 to (Length(centroids) - 1) do begin
  assign_clusters_dist := distance_sq(data[assign_clusters_i], centroids[assign_clusters_j]);
  if assign_clusters_dist < assign_clusters_best then begin
  assign_clusters_best := assign_clusters_dist;
  assign_clusters_best_idx := assign_clusters_j;
end;
end;
  assign_clusters_assignments := concat(assign_clusters_assignments, IntArray([assign_clusters_best_idx]));
end;
  exit(assign_clusters_assignments);
end;
function revise_centroids(data: RealArrayArray; k: integer; assignment: IntArray): RealArrayArray;
var
  revise_centroids_dim: integer;
  revise_centroids_sums: array of RealArray;
  revise_centroids_counts: array of integer;
  revise_centroids_i: int64;
  revise_centroids_row: array of real;
  revise_centroids_j: int64;
  revise_centroids_c: integer;
  revise_centroids_centroids: array of RealArray;
  revise_centroids_j_21: int64;
  revise_centroids_j_22: int64;
begin
  revise_centroids_dim := Length(data[0]);
  revise_centroids_sums := [];
  revise_centroids_counts := [];
  for revise_centroids_i := 0 to (k - 1) do begin
  revise_centroids_row := [];
  for revise_centroids_j := 0 to (revise_centroids_dim - 1) do begin
  revise_centroids_row := concat(revise_centroids_row, [0]);
end;
  revise_centroids_sums := concat(revise_centroids_sums, [revise_centroids_row]);
  revise_centroids_counts := concat(revise_centroids_counts, IntArray([0]));
end;
  for revise_centroids_i := 0 to (Length(data) - 1) do begin
  revise_centroids_c := assignment[revise_centroids_i];
  revise_centroids_counts[revise_centroids_c] := revise_centroids_counts[revise_centroids_c] + 1;
  for revise_centroids_j := 0 to (revise_centroids_dim - 1) do begin
  revise_centroids_sums[revise_centroids_c][revise_centroids_j] := revise_centroids_sums[revise_centroids_c][revise_centroids_j] + data[revise_centroids_i][revise_centroids_j];
end;
end;
  revise_centroids_centroids := [];
  for revise_centroids_i := 0 to (k - 1) do begin
  revise_centroids_row := [];
  if revise_centroids_counts[revise_centroids_i] > 0 then begin
  for revise_centroids_j_21 := 0 to (revise_centroids_dim - 1) do begin
  revise_centroids_row := concat(revise_centroids_row, [revise_centroids_sums[revise_centroids_i][revise_centroids_j_21] / Double(revise_centroids_counts[revise_centroids_i])]);
end;
end else begin
  for revise_centroids_j_22 := 0 to (revise_centroids_dim - 1) do begin
  revise_centroids_row := concat(revise_centroids_row, [0]);
end;
end;
  revise_centroids_centroids := concat(revise_centroids_centroids, [revise_centroids_row]);
end;
  exit(revise_centroids_centroids);
end;
function compute_heterogeneity(data: RealArrayArray; centroids: RealArrayArray; assignment: IntArray): real;
var
  compute_heterogeneity_total: real;
  compute_heterogeneity_i: int64;
  compute_heterogeneity_c: integer;
begin
  compute_heterogeneity_total := 0;
  for compute_heterogeneity_i := 0 to (Length(data) - 1) do begin
  compute_heterogeneity_c := assignment[compute_heterogeneity_i];
  compute_heterogeneity_total := compute_heterogeneity_total + distance_sq(data[compute_heterogeneity_i], centroids[compute_heterogeneity_c]);
end;
  exit(compute_heterogeneity_total);
end;
function lists_equal(a: IntArray; b: IntArray): boolean;
var
  lists_equal_i: int64;
begin
  if Length(a) <> Length(b) then begin
  exit(false);
end;
  for lists_equal_i := 0 to (Length(a) - 1) do begin
  if a[lists_equal_i] <> b[lists_equal_i] then begin
  exit(false);
end;
end;
  exit(true);
end;
function kmeans(data: RealArrayArray; k: integer; initial_centroids: RealArrayArray; max_iter: integer): KMeansResult;
var
  kmeans_centroids: array of RealArray;
  kmeans_assignment: array of integer;
  kmeans_prev: array of integer;
  kmeans_heterogeneity: array of real;
  kmeans_iter: integer;
  kmeans_h: real;
begin
  kmeans_centroids := initial_centroids;
  kmeans_assignment := [];
  kmeans_prev := [];
  kmeans_heterogeneity := [];
  kmeans_iter := 0;
  while kmeans_iter < max_iter do begin
  kmeans_assignment := assign_clusters(data, kmeans_centroids);
  kmeans_centroids := revise_centroids(data, k, kmeans_assignment);
  kmeans_h := compute_heterogeneity(data, kmeans_centroids, kmeans_assignment);
  kmeans_heterogeneity := concat(kmeans_heterogeneity, [kmeans_h]);
  if (kmeans_iter > 0) and lists_equal(kmeans_prev, kmeans_assignment) then begin
  break;
end;
  kmeans_prev := kmeans_assignment;
  kmeans_iter := kmeans_iter + 1;
end;
  exit(makeKMeansResult(kmeans_centroids, kmeans_assignment, kmeans_heterogeneity));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  data := [[1, 2], [1.5, 1.8], [5, 8], [8, 8], [1, 0.6], [9, 11]];
  k := 3;
  initial_centroids := [data[0], data[2], data[5]];
  result_ := kmeans(data, k, initial_centroids, 10);
  writeln(list_list_real_to_str(result_.centroids));
  writeln(list_int_to_str(result_.assignments));
  writeln(list_real_to_str(result_.heterogeneity));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
