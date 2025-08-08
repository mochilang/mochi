{$mode objfpc}
program Main;
uses SysUtils;
type Node = record
  data: integer;
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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  small: NodeArray;
  medium: NodeArray;
  acc: IntArray;
  nodes: NodeArray;
  index: integer;
function makeNode(data: integer; left: integer; right: integer): Node; forward;
function inorder(nodes: NodeArray; index: integer; acc: IntArray): IntArray; forward;
function size(nodes: NodeArray; index: integer): integer; forward;
function depth(nodes: NodeArray; index: integer): integer; forward;
function is_full(nodes: NodeArray; index: integer): boolean; forward;
function small_tree(): NodeArray; forward;
function medium_tree(): NodeArray; forward;
function makeNode(data: integer; left: integer; right: integer): Node;
begin
  Result.data := data;
  Result.left := left;
  Result.right := right;
end;
function inorder(nodes: NodeArray; index: integer; acc: IntArray): IntArray;
var
  inorder_node_var: Node;
  inorder_res: IntArray;
begin
  if index = (0 - 1) then begin
  exit(acc);
end;
  inorder_node_var := nodes[index];
  inorder_res := inorder(nodes, inorder_node_var.left, acc);
  inorder_res := concat(inorder_res, IntArray([inorder_node_var.data]));
  inorder_res := inorder(nodes, inorder_node_var.right, inorder_res);
  exit(inorder_res);
end;
function size(nodes: NodeArray; index: integer): integer;
var
  size_node_var: Node;
begin
  if index = (0 - 1) then begin
  exit(0);
end;
  size_node_var := nodes[index];
  exit((1 + size(nodes, size_node_var.left)) + size(nodes, size_node_var.right));
end;
function depth(nodes: NodeArray; index: integer): integer;
var
  depth_node_var: Node;
  depth_left_depth: integer;
  depth_right_depth: integer;
begin
  if index = (0 - 1) then begin
  exit(0);
end;
  depth_node_var := nodes[index];
  depth_left_depth := depth(nodes, depth_node_var.left);
  depth_right_depth := depth(nodes, depth_node_var.right);
  if depth_left_depth > depth_right_depth then begin
  exit(depth_left_depth + 1);
end;
  exit(depth_right_depth + 1);
end;
function is_full(nodes: NodeArray; index: integer): boolean;
var
  is_full_node_var: Node;
begin
  if index = (0 - 1) then begin
  exit(true);
end;
  is_full_node_var := nodes[index];
  if (is_full_node_var.left = (0 - 1)) and (is_full_node_var.right = (0 - 1)) then begin
  exit(true);
end;
  if (is_full_node_var.left <> (0 - 1)) and (is_full_node_var.right <> (0 - 1)) then begin
  exit(is_full(nodes, is_full_node_var.left) and is_full(nodes, is_full_node_var.right));
end;
  exit(false);
end;
function small_tree(): NodeArray;
var
  small_tree_arr: array of Node;
begin
  small_tree_arr := [];
  small_tree_arr := concat(small_tree_arr, [makeNode(2, 1, 2)]);
  small_tree_arr := concat(small_tree_arr, [makeNode(1, 0 - 1, 0 - 1)]);
  small_tree_arr := concat(small_tree_arr, [makeNode(3, 0 - 1, 0 - 1)]);
  exit(small_tree_arr);
end;
function medium_tree(): NodeArray;
var
  medium_tree_arr: array of Node;
begin
  medium_tree_arr := [];
  medium_tree_arr := concat(medium_tree_arr, [makeNode(4, 1, 4)]);
  medium_tree_arr := concat(medium_tree_arr, [makeNode(2, 2, 3)]);
  medium_tree_arr := concat(medium_tree_arr, [makeNode(1, 0 - 1, 0 - 1)]);
  medium_tree_arr := concat(medium_tree_arr, [makeNode(3, 0 - 1, 0 - 1)]);
  medium_tree_arr := concat(medium_tree_arr, [makeNode(5, 0 - 1, 5)]);
  medium_tree_arr := concat(medium_tree_arr, [makeNode(6, 0 - 1, 6)]);
  medium_tree_arr := concat(medium_tree_arr, [makeNode(7, 0 - 1, 0 - 1)]);
  exit(medium_tree_arr);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  small := small_tree();
  writeln(size(small, 0));
  show_list(inorder(small, 0, []));
  writeln(depth(small, 0));
  writeln(Ord(is_full(small, 0)));
  medium := medium_tree();
  writeln(size(medium, 0));
  show_list(inorder(medium, 0, []));
  writeln(depth(medium, 0));
  writeln(Ord(is_full(medium, 0)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
