{$mode objfpc}
program Main;
uses SysUtils;
type DisjointSet = record
  set_counts: array of integer;
  max_set: integer;
  ranks: array of integer;
  parents: array of integer;
end;
type IntArray = array of integer;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  ds: DisjointSet;
  src: integer;
  idx: integer;
  xs: IntArray;
  set_counts: IntArray;
  dst: integer;
function makeDisjointSet(set_counts: IntArray; max_set: integer; ranks: IntArray; parents: IntArray): DisjointSet; forward;
function max_list(xs: IntArray): integer; forward;
function disjoint_set_new(set_counts: IntArray): DisjointSet; forward;
function get_parent(ds: DisjointSet; idx: integer): integer; forward;
function merge(ds: DisjointSet; src: integer; dst: integer): boolean; forward;
function makeDisjointSet(set_counts: IntArray; max_set: integer; ranks: IntArray; parents: IntArray): DisjointSet;
begin
  Result.set_counts := set_counts;
  Result.max_set := max_set;
  Result.ranks := ranks;
  Result.parents := parents;
end;
function max_list(xs: IntArray): integer;
var
  max_list_m: integer;
  max_list_i: integer;
begin
  max_list_m := xs[0];
  max_list_i := 1;
  while max_list_i < Length(xs) do begin
  if xs[max_list_i] > max_list_m then begin
  max_list_m := xs[max_list_i];
end;
  max_list_i := max_list_i + 1;
end;
  exit(max_list_m);
end;
function disjoint_set_new(set_counts: IntArray): DisjointSet;
var
  disjoint_set_new_max_set: integer;
  disjoint_set_new_num_sets: integer;
  disjoint_set_new_ranks: array of integer;
  disjoint_set_new_parents: array of integer;
  disjoint_set_new_i: integer;
begin
  disjoint_set_new_max_set := max_list(set_counts);
  disjoint_set_new_num_sets := Length(set_counts);
  disjoint_set_new_ranks := [];
  disjoint_set_new_parents := [];
  disjoint_set_new_i := 0;
  while disjoint_set_new_i < disjoint_set_new_num_sets do begin
  disjoint_set_new_ranks := concat(disjoint_set_new_ranks, IntArray([1]));
  disjoint_set_new_parents := concat(disjoint_set_new_parents, IntArray([disjoint_set_new_i]));
  disjoint_set_new_i := disjoint_set_new_i + 1;
end;
  exit(makeDisjointSet(set_counts, disjoint_set_new_max_set, disjoint_set_new_ranks, disjoint_set_new_parents));
end;
function get_parent(ds: DisjointSet; idx: integer): integer;
var
  get_parent_parents: array of integer;
begin
  if ds.parents[idx] = idx then begin
  exit(idx);
end;
  get_parent_parents := ds.parents;
  get_parent_parents[idx] := get_parent(ds, get_parent_parents[idx]);
  ds.parents := get_parent_parents;
  exit(ds.parents[idx]);
end;
function merge(ds: DisjointSet; src: integer; dst: integer): boolean;
var
  merge_src_parent: integer;
  merge_dst_parent: integer;
  merge_counts: array of integer;
  merge_parents: array of integer;
  merge_ranks: array of integer;
  merge_joined: integer;
begin
  merge_src_parent := get_parent(ds, src);
  merge_dst_parent := get_parent(ds, dst);
  if merge_src_parent = merge_dst_parent then begin
  exit(false);
end;
  if ds.ranks[merge_dst_parent] >= ds.ranks[merge_src_parent] then begin
  merge_counts := ds.set_counts;
  merge_counts[merge_dst_parent] := merge_counts[merge_dst_parent] + merge_counts[merge_src_parent];
  merge_counts[merge_src_parent] := 0;
  ds.set_counts := merge_counts;
  merge_parents := ds.parents;
  merge_parents[merge_src_parent] := merge_dst_parent;
  ds.parents := merge_parents;
  if ds.ranks[merge_dst_parent] = ds.ranks[merge_src_parent] then begin
  merge_ranks := ds.ranks;
  merge_ranks[merge_dst_parent] := merge_ranks[merge_dst_parent] + 1;
  ds.ranks := merge_ranks;
end;
  merge_joined := ds.set_counts[merge_dst_parent];
  if merge_joined > ds.max_set then begin
  ds.max_set := merge_joined;
end;
end else begin
  merge_counts := ds.set_counts;
  merge_counts[merge_src_parent] := merge_counts[merge_src_parent] + merge_counts[merge_dst_parent];
  merge_counts[merge_dst_parent] := 0;
  ds.set_counts := merge_counts;
  merge_parents := ds.parents;
  merge_parents[merge_dst_parent] := merge_src_parent;
  ds.parents := merge_parents;
  merge_joined := ds.set_counts[merge_src_parent];
  if merge_joined > ds.max_set then begin
  ds.max_set := merge_joined;
end;
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  ds := disjoint_set_new([1, 1, 1]);
  writeln(Ord(merge(ds, 1, 2)));
  writeln(Ord(merge(ds, 0, 2)));
  writeln(Ord(merge(ds, 0, 1)));
  writeln(get_parent(ds, 0));
  writeln(get_parent(ds, 1));
  writeln(ds.max_set);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
