{$mode objfpc}
program Main;
uses SysUtils, Math;
type Node = record
  minn: integer;
  maxx: integer;
  map_left: array of integer;
  left: integer;
  right: integer;
end;
type NodeArray = array of Node;
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
  nodes: array of Node;
  test_array: array of integer;
  root: integer;
  arr: IntArray;
  length_: integer;
  index: integer;
  num: integer;
  end_: integer;
  value: integer;
  start_num: integer;
  start: integer;
  node_idx: integer;
  end_num: integer;
function makeNode(minn: integer; maxx: integer; map_left: IntArray; left: integer; right: integer): Node; forward;
function make_list(length_: integer; value: integer): IntArray; forward;
function min_list(arr: IntArray): integer; forward;
function max_list(arr: IntArray): integer; forward;
function build_tree(arr: IntArray): integer; forward;
function rank_till_index(node_idx: integer; num: integer; index: integer): integer; forward;
function rank(node_idx: integer; num: integer; start: integer; end_: integer): integer; forward;
function quantile(node_idx: integer; index: integer; start: integer; end_: integer): integer; forward;
function range_counting(node_idx: integer; start: integer; end_: integer; start_num: integer; end_num: integer): integer; forward;
function makeNode(minn: integer; maxx: integer; map_left: IntArray; left: integer; right: integer): Node;
begin
  Result.minn := minn;
  Result.maxx := maxx;
  Result.map_left := map_left;
  Result.left := left;
  Result.right := right;
end;
function make_list(length_: integer; value: integer): IntArray;
var
  make_list_lst: array of integer;
  make_list_i: integer;
begin
  make_list_lst := [];
  make_list_i := 0;
  while make_list_i < length_ do begin
  make_list_lst := concat(make_list_lst, IntArray([value]));
  make_list_i := make_list_i + 1;
end;
  exit(make_list_lst);
end;
function min_list(arr: IntArray): integer;
var
  min_list_m: integer;
  min_list_i: integer;
begin
  min_list_m := arr[0];
  min_list_i := 1;
  while min_list_i < Length(arr) do begin
  if arr[min_list_i] < min_list_m then begin
  min_list_m := arr[min_list_i];
end;
  min_list_i := min_list_i + 1;
end;
  exit(min_list_m);
end;
function max_list(arr: IntArray): integer;
var
  max_list_m: integer;
  max_list_i: integer;
begin
  max_list_m := arr[0];
  max_list_i := 1;
  while max_list_i < Length(arr) do begin
  if arr[max_list_i] > max_list_m then begin
  max_list_m := arr[max_list_i];
end;
  max_list_i := max_list_i + 1;
end;
  exit(max_list_m);
end;
function build_tree(arr: IntArray): integer;
var
  build_tree_n: Node;
  build_tree_pivot: integer;
  build_tree_left_arr: array of integer;
  build_tree_right_arr: array of integer;
  build_tree_i: integer;
  build_tree_num: integer;
  build_tree_ml: array of integer;
begin
  build_tree_n := makeNode(min_list(arr), max_list(arr), make_list(Length(arr), 0), -1, -1);
  if build_tree_n.minn = build_tree_n.maxx then begin
  nodes := concat(nodes, [build_tree_n]);
  exit(Length(nodes) - 1);
end;
  build_tree_pivot := (build_tree_n.minn + build_tree_n.maxx) div 2;
  build_tree_left_arr := [];
  build_tree_right_arr := [];
  build_tree_i := 0;
  while build_tree_i < Length(arr) do begin
  build_tree_num := arr[build_tree_i];
  if build_tree_num <= build_tree_pivot then begin
  build_tree_left_arr := concat(build_tree_left_arr, IntArray([build_tree_num]));
end else begin
  build_tree_right_arr := concat(build_tree_right_arr, IntArray([build_tree_num]));
end;
  build_tree_ml := build_tree_n.map_left;
  build_tree_ml[build_tree_i] := Length(build_tree_left_arr);
  build_tree_n.map_left := build_tree_ml;
  build_tree_i := build_tree_i + 1;
end;
  if Length(build_tree_left_arr) > 0 then begin
  build_tree_n.left := build_tree(build_tree_left_arr);
end;
  if Length(build_tree_right_arr) > 0 then begin
  build_tree_n.right := build_tree(build_tree_right_arr);
end;
  nodes := concat(nodes, [build_tree_n]);
  exit(Length(nodes) - 1);
end;
function rank_till_index(node_idx: integer; num: integer; index: integer): integer;
var
  rank_till_index_node_var: Node;
  rank_till_index_pivot: integer;
begin
  if (index < 0) or (node_idx < 0) then begin
  exit(0);
end;
  rank_till_index_node_var := nodes[node_idx];
  if rank_till_index_node_var.minn = rank_till_index_node_var.maxx then begin
  if rank_till_index_node_var.minn = num then begin
  exit(index + 1);
end else begin
  exit(0);
end;
end;
  rank_till_index_pivot := (rank_till_index_node_var.minn + rank_till_index_node_var.maxx) div 2;
  if num <= rank_till_index_pivot then begin
  exit(rank_till_index(rank_till_index_node_var.left, num, rank_till_index_node_var.map_left[index] - 1));
end else begin
  exit(rank_till_index(rank_till_index_node_var.right, num, index - rank_till_index_node_var.map_left[index]));
end;
end;
function rank(node_idx: integer; num: integer; start: integer; end_: integer): integer;
var
  rank_rank_till_end: integer;
  rank_rank_before_start: integer;
begin
  if start > end_ then begin
  exit(0);
end;
  rank_rank_till_end := rank_till_index(node_idx, num, end_);
  rank_rank_before_start := rank_till_index(node_idx, num, start - 1);
  exit(rank_rank_till_end - rank_rank_before_start);
end;
function quantile(node_idx: integer; index: integer; start: integer; end_: integer): integer;
var
  quantile_node_var: Node;
  quantile_left_start: integer;
  quantile_num_left: integer;
begin
  if ((index > (end_ - start)) or (start > end_)) or (node_idx < 0) then begin
  exit(-1);
end;
  quantile_node_var := nodes[node_idx];
  if quantile_node_var.minn = quantile_node_var.maxx then begin
  exit(quantile_node_var.minn);
end;
  if start = 0 then begin
  quantile_left_start := 0;
end else begin
  quantile_left_start := quantile_node_var.map_left[start - 1];
end;
  quantile_num_left := quantile_node_var.map_left[end_] - quantile_left_start;
  if quantile_num_left > index then begin
  exit(quantile(quantile_node_var.left, index, quantile_left_start, quantile_node_var.map_left[end_] - 1));
end else begin
  exit(quantile(quantile_node_var.right, index - quantile_num_left, start - quantile_left_start, end_ - quantile_node_var.map_left[end_]));
end;
end;
function range_counting(node_idx: integer; start: integer; end_: integer; start_num: integer; end_num: integer): integer;
var
  range_counting_node_var: Node;
  range_counting_left: integer;
  range_counting_right: integer;
begin
  if ((start > end_) or (node_idx < 0)) or (start_num > end_num) then begin
  exit(0);
end;
  range_counting_node_var := nodes[node_idx];
  if (range_counting_node_var.minn > end_num) or (range_counting_node_var.maxx < start_num) then begin
  exit(0);
end;
  if (start_num <= range_counting_node_var.minn) and (range_counting_node_var.maxx <= end_num) then begin
  exit((end_ - start) + 1);
end;
  range_counting_left := range_counting(range_counting_node_var.left, IfThen(start = 0, 0, range_counting_node_var.map_left[start - 1]), range_counting_node_var.map_left[end_] - 1, start_num, end_num);
  range_counting_right := range_counting(range_counting_node_var.right, start - IfThen(start = 0, 0, range_counting_node_var.map_left[start - 1]), end_ - range_counting_node_var.map_left[end_], start_num, end_num);
  exit(range_counting_left + range_counting_right);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  nodes := [];
  test_array := [2, 1, 4, 5, 6, 0, 8, 9, 1, 2, 0, 6, 4, 2, 0, 6, 5, 3, 2, 7];
  root := build_tree(test_array);
  writeln('rank_till_index 6 at 6 -> ' + IntToStr(rank_till_index(root, 6, 6)));
  writeln('rank 6 in [3,13] -> ' + IntToStr(rank(root, 6, 3, 13)));
  writeln('quantile index 2 in [2,5] -> ' + IntToStr(quantile(root, 2, 2, 5)));
  writeln('range_counting [3,7] in [1,10] -> ' + IntToStr(range_counting(root, 1, 10, 3, 7)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
