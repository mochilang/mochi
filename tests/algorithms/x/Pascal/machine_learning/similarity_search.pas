{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Neighbor = record
  vector: array of real;
  distance: real;
end;
type RealArray = array of real;
type NeighborArray = array of Neighbor;
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
  dataset: array of array of real;
  value_array: array of array of real;
  neighbors: NeighborArray;
  k: integer;
  n: Neighbor;
  a: RealArray;
  x: real;
  b: RealArray;
function makeNeighbor(vector: RealArray; distance: real): Neighbor; forward;
function sqrt(x: real): real; forward;
function euclidean(a: RealArray; b: RealArray): real; forward;
function similarity_search(dataset: RealArrayArray; value_array: RealArrayArray): NeighborArray; forward;
function cosine_similarity(a: RealArray; b: RealArray): real; forward;
function makeNeighbor(vector: RealArray; distance: real): Neighbor;
begin
  Result.vector := vector;
  Result.distance := distance;
end;
function sqrt(x: real): real;
var
  sqrt_guess: real;
  sqrt_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrt_guess := x;
  sqrt_i := 0;
  while sqrt_i < 10 do begin
  sqrt_guess := (sqrt_guess + (x / sqrt_guess)) / 2;
  sqrt_i := sqrt_i + 1;
end;
  exit(sqrt_guess);
end;
function euclidean(a: RealArray; b: RealArray): real;
var
  euclidean_sum: real;
  euclidean_i: integer;
  euclidean_diff: real;
  euclidean_res: real;
begin
  euclidean_sum := 0;
  euclidean_i := 0;
  while euclidean_i < Length(a) do begin
  euclidean_diff := a[euclidean_i] - b[euclidean_i];
  euclidean_sum := euclidean_sum + (euclidean_diff * euclidean_diff);
  euclidean_i := euclidean_i + 1;
end;
  euclidean_res := sqrt(euclidean_sum);
  exit(euclidean_res);
end;
function similarity_search(dataset: RealArrayArray; value_array: RealArrayArray): NeighborArray;
var
  similarity_search_dim: integer;
  similarity_search_result_: array of Neighbor;
  similarity_search_i: integer;
  similarity_search_value: array of real;
  similarity_search_dist: real;
  similarity_search_vec: array of real;
  similarity_search_j: integer;
  similarity_search_d: real;
  similarity_search_nb: Neighbor;
begin
  similarity_search_dim := Length(dataset[0]);
  if similarity_search_dim <> Length(value_array[0]) then begin
  exit([]);
end;
  similarity_search_result_ := [];
  similarity_search_i := 0;
  while similarity_search_i < Length(value_array) do begin
  similarity_search_value := value_array[similarity_search_i];
  similarity_search_dist := euclidean(similarity_search_value, dataset[0]);
  similarity_search_vec := dataset[0];
  similarity_search_j := 1;
  while similarity_search_j < Length(dataset) do begin
  similarity_search_d := euclidean(similarity_search_value, dataset[similarity_search_j]);
  if similarity_search_d < similarity_search_dist then begin
  similarity_search_dist := similarity_search_d;
  similarity_search_vec := dataset[similarity_search_j];
end;
  similarity_search_j := similarity_search_j + 1;
end;
  similarity_search_nb := makeNeighbor(similarity_search_vec, similarity_search_dist);
  similarity_search_result_ := concat(similarity_search_result_, [similarity_search_nb]);
  similarity_search_i := similarity_search_i + 1;
end;
  exit(similarity_search_result_);
end;
function cosine_similarity(a: RealArray; b: RealArray): real;
var
  cosine_similarity_dot: real;
  cosine_similarity_norm_a: real;
  cosine_similarity_norm_b: real;
  cosine_similarity_i: integer;
begin
  cosine_similarity_dot := 0;
  cosine_similarity_norm_a := 0;
  cosine_similarity_norm_b := 0;
  cosine_similarity_i := 0;
  while cosine_similarity_i < Length(a) do begin
  cosine_similarity_dot := cosine_similarity_dot + (a[cosine_similarity_i] * b[cosine_similarity_i]);
  cosine_similarity_norm_a := cosine_similarity_norm_a + (a[cosine_similarity_i] * a[cosine_similarity_i]);
  cosine_similarity_norm_b := cosine_similarity_norm_b + (b[cosine_similarity_i] * b[cosine_similarity_i]);
  cosine_similarity_i := cosine_similarity_i + 1;
end;
  if (cosine_similarity_norm_a = 0) or (cosine_similarity_norm_b = 0) then begin
  exit(0);
end;
  exit(cosine_similarity_dot / (sqrt(cosine_similarity_norm_a) * sqrt(cosine_similarity_norm_b)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  dataset := [[0, 0, 0], [1, 1, 1], [2, 2, 2]];
  value_array := [[0, 0, 0], [0, 0, 1]];
  neighbors := similarity_search(dataset, value_array);
  k := 0;
  while k < Length(neighbors) do begin
  n := neighbors[k];
  writeln(((('[' + list_real_to_str(n.vector)) + ', ') + FloatToStr(n.distance)) + ']');
  k := k + 1;
end;
  writeln(FloatToStr(cosine_similarity([1, 2], [6, 32])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
