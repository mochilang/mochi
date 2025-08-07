{$mode objfpc}
program Main;
uses SysUtils;
type Node = record
  start: integer;
  end_: integer;
  val: integer;
  mid: integer;
  left: integer;
  right: integer;
end;
type BuildResult = record
  nodes: array of Node;
  idx: integer;
end;
type SegmentTree = record
  arr: array of integer;
  op: integer;
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
  arr: array of integer;
  op: integer;
  tree: SegmentTree;
  collection: IntArray;
  b: integer;
  nodes: NodeArray;
  node_var: Node;
  start: integer;
  a: integer;
  i: integer;
  end_: integer;
  val: integer;
  j: integer;
function makeSegmentTree(arr: IntArray; op: integer): SegmentTree; forward;
function makeBuildResult(nodes: NodeArray; idx: integer): BuildResult; forward;
function makeNode(start: integer; end_: integer; val: integer; mid: integer; left: integer; right: integer): Node; forward;
function combine(a: integer; b: integer; op: integer): integer; forward;
function build_tree(nodes: NodeArray; arr: IntArray; start: integer; end_: integer; op: integer): BuildResult; forward;
function new_segment_tree(collection: IntArray; op: integer): SegmentTree; forward;
function update(tree: SegmentTree; i: integer; val: integer): SegmentTree; forward;
function query_range(tree: SegmentTree; i: integer; j: integer): integer; forward;
function traverse(tree: SegmentTree): NodeArray; forward;
function node_to_string(node_var: Node): string; forward;
procedure print_traverse(tree: SegmentTree); forward;
function makeSegmentTree(arr: IntArray; op: integer): SegmentTree;
begin
  Result.arr := arr;
  Result.op := op;
end;
function makeBuildResult(nodes: NodeArray; idx: integer): BuildResult;
begin
  Result.nodes := nodes;
  Result.idx := idx;
end;
function makeNode(start: integer; end_: integer; val: integer; mid: integer; left: integer; right: integer): Node;
begin
  Result.start := start;
  Result.end_ := end_;
  Result.val := val;
  Result.mid := mid;
  Result.left := left;
  Result.right := right;
end;
function combine(a: integer; b: integer; op: integer): integer;
begin
  if op = 0 then begin
  exit(a + b);
end;
  if op = 1 then begin
  if a > b then begin
  exit(a);
end;
  exit(b);
end;
  if a < b then begin
  exit(a);
end;
  exit(b);
end;
function build_tree(nodes: NodeArray; arr: IntArray; start: integer; end_: integer; op: integer): BuildResult;
var
  build_tree_node_var: Node;
  build_tree_new_nodes: array of Node;
  build_tree_mid: integer;
  build_tree_left_res: BuildResult;
  build_tree_right_res: BuildResult;
  build_tree_left_node: Node;
  build_tree_right_node: Node;
  build_tree_val: integer;
  build_tree_parent: Node;
begin
  if start = end_ then begin
  build_tree_node_var := makeNode(start, end_, arr[start], start, -1, -1);
  build_tree_new_nodes := concat(nodes, [build_tree_node_var]);
  exit(makeBuildResult(build_tree_new_nodes, Length(build_tree_new_nodes) - 1));
end;
  build_tree_mid := (start + end_) div 2;
  build_tree_left_res := build_tree(nodes, arr, start, build_tree_mid, op);
  build_tree_right_res := build_tree(build_tree_left_res.nodes, arr, build_tree_mid + 1, end_, op);
  build_tree_left_node := build_tree_right_res.nodes[build_tree_left_res.idx];
  build_tree_right_node := build_tree_right_res.nodes[build_tree_right_res.idx];
  build_tree_val := combine(build_tree_left_node.val, build_tree_right_node.val, op);
  build_tree_parent := makeNode(start, end_, build_tree_val, build_tree_mid, build_tree_left_res.idx, build_tree_right_res.idx);
  build_tree_new_nodes := concat(build_tree_right_res.nodes, [build_tree_parent]);
  exit(makeBuildResult(build_tree_new_nodes, Length(build_tree_new_nodes) - 1));
end;
function new_segment_tree(collection: IntArray; op: integer): SegmentTree;
begin
  exit(makeSegmentTree(collection, op));
end;
function update(tree: SegmentTree; i: integer; val: integer): SegmentTree;
var
  update_new_arr: array of integer;
  update_idx: integer;
begin
  update_new_arr := [];
  update_idx := 0;
  while update_idx < Length(tree.arr) do begin
  if update_idx = i then begin
  update_new_arr := concat(update_new_arr, IntArray([val]));
end else begin
  update_new_arr := concat(update_new_arr, IntArray([tree.arr[update_idx]]));
end;
  update_idx := update_idx + 1;
end;
  exit(makeSegmentTree(update_new_arr, tree.op));
end;
function query_range(tree: SegmentTree; i: integer; j: integer): integer;
var
  query_range_result_: integer;
  query_range_idx: integer;
begin
  query_range_result_ := tree.arr[i];
  query_range_idx := i + 1;
  while query_range_idx <= j do begin
  query_range_result_ := combine(query_range_result_, tree.arr[query_range_idx], tree.op);
  query_range_idx := query_range_idx + 1;
end;
  exit(query_range_result_);
end;
function traverse(tree: SegmentTree): NodeArray;
var
  traverse_res: BuildResult;
begin
  if Length(tree.arr) = 0 then begin
  exit([]);
end;
  traverse_res := build_tree([], tree.arr, 0, Length(tree.arr) - 1, tree.op);
  exit(traverse_res.nodes);
end;
function node_to_string(node_var: Node): string;
begin
  exit(((((('SegmentTreeNode(start=' + IntToStr(node_var.start)) + ', end=') + IntToStr(node_var.end_)) + ', val=') + IntToStr(node_var.val)) + ')');
end;
procedure print_traverse(tree: SegmentTree);
var
  print_traverse_nodes: NodeArray;
  print_traverse_i: integer;
begin
  print_traverse_nodes := traverse(tree);
  print_traverse_i := 0;
  while print_traverse_i < Length(print_traverse_nodes) do begin
  writeln(node_to_string(print_traverse_nodes[print_traverse_i]));
  print_traverse_i := print_traverse_i + 1;
end;
  writeln('');
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  arr := [2, 1, 5, 3, 4];
  for op in [0, 1, 2] do begin
  writeln('**************************************************');
  tree := new_segment_tree(arr, op);
  print_traverse(tree);
  tree := update(tree, 1, 5);
  print_traverse(tree);
  writeln(query_range(tree, 3, 4));
  writeln(query_range(tree, 2, 2));
  writeln(query_range(tree, 1, 3));
  writeln('');
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
