{$mode objfpc}
program Main;
uses SysUtils;
type RealArray = array of real;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
type RealArrayArray = array of RealArray;
type KMeansResult = record
  centroids: array of RealArray;
  assignments: array of integer;
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
  iterations: integer;
  k: integer;
  vectors: RealArrayArray;
  a: RealArray;
  b: RealArray;
function makeKMeansResult(centroids: RealArrayArray; assignments: IntArray): KMeansResult; forward;
function distance_sq(a: RealArray; b: RealArray): real; forward;
function mean(vectors: RealArrayArray): RealArray; forward;
function k_means(vectors: RealArrayArray; k: integer; iterations: integer): KMeansResult; forward;
procedure main(); forward;
function makeKMeansResult(centroids: RealArrayArray; assignments: IntArray): KMeansResult;
begin
  Result.centroids := centroids;
  Result.assignments := assignments;
end;
function distance_sq(a: RealArray; b: RealArray): real;
var
  distance_sq_sum: real;
  distance_sq_i: integer;
  distance_sq_diff: real;
begin
  distance_sq_sum := 0;
  distance_sq_i := 0;
  while distance_sq_i < Length(a) do begin
  distance_sq_diff := a[distance_sq_i] - b[distance_sq_i];
  distance_sq_sum := distance_sq_sum + (distance_sq_diff * distance_sq_diff);
  distance_sq_i := distance_sq_i + 1;
end;
  exit(distance_sq_sum);
end;
function mean(vectors: RealArrayArray): RealArray;
var
  mean_dim: integer;
  mean_res: array of real;
  mean_i: integer;
  mean_total: real;
  mean_j: integer;
begin
  mean_dim := Length(vectors[0]);
  mean_res := [];
  mean_i := 0;
  while mean_i < mean_dim do begin
  mean_total := 0;
  mean_j := 0;
  while mean_j < Length(vectors) do begin
  mean_total := mean_total + vectors[mean_j][mean_i];
  mean_j := mean_j + 1;
end;
  mean_res := concat(mean_res, [mean_total / Length(vectors)]);
  mean_i := mean_i + 1;
end;
  exit(mean_res);
end;
function k_means(vectors: RealArrayArray; k: integer; iterations: integer): KMeansResult;
var
  k_means_centroids: array of RealArray;
  k_means_i: integer;
  k_means_assignments: array of integer;
  k_means_n: integer;
  k_means_it: integer;
  k_means_v: integer;
  k_means_best: integer;
  k_means_bestDist: real;
  k_means_c: integer;
  k_means_d: real;
  k_means_cIdx: integer;
  k_means_cluster: array of RealArray;
  k_means_v2: integer;
begin
  k_means_centroids := [];
  k_means_i := 0;
  while k_means_i < k do begin
  k_means_centroids := concat(k_means_centroids, [vectors[k_means_i]]);
  k_means_i := k_means_i + 1;
end;
  k_means_assignments := [];
  k_means_n := Length(vectors);
  k_means_i := 0;
  while k_means_i < k_means_n do begin
  k_means_assignments := concat(k_means_assignments, IntArray([0]));
  k_means_i := k_means_i + 1;
end;
  k_means_it := 0;
  while k_means_it < iterations do begin
  k_means_v := 0;
  while k_means_v < k_means_n do begin
  k_means_best := 0;
  k_means_bestDist := distance_sq(vectors[k_means_v], k_means_centroids[0]);
  k_means_c := 1;
  while k_means_c < k do begin
  k_means_d := distance_sq(vectors[k_means_v], k_means_centroids[k_means_c]);
  if k_means_d < k_means_bestDist then begin
  k_means_bestDist := k_means_d;
  k_means_best := k_means_c;
end;
  k_means_c := k_means_c + 1;
end;
  k_means_assignments[k_means_v] := k_means_best;
  k_means_v := k_means_v + 1;
end;
  k_means_cIdx := 0;
  while k_means_cIdx < k do begin
  k_means_cluster := [];
  k_means_v2 := 0;
  while k_means_v2 < k_means_n do begin
  if k_means_assignments[k_means_v2] = k_means_cIdx then begin
  k_means_cluster := concat(k_means_cluster, [vectors[k_means_v2]]);
end;
  k_means_v2 := k_means_v2 + 1;
end;
  if Length(k_means_cluster) > 0 then begin
  k_means_centroids[k_means_cIdx] := mean(k_means_cluster);
end;
  k_means_cIdx := k_means_cIdx + 1;
end;
  k_means_it := k_means_it + 1;
end;
  exit(makeKMeansResult(k_means_centroids, k_means_assignments));
end;
procedure main();
var
  main_vectors: array of array of real;
  main_result_: KMeansResult;
begin
  main_vectors := [[1, 2], [1.5, 1.8], [5, 8], [8, 8], [1, 0.6], [9, 11]];
  main_result_ := k_means(main_vectors, 2, 5);
  writeln(list_int_to_str(main_result_.centroids));
  writeln(list_int_to_str(main_result_.assignments));
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
end.
