{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Node = record
  value: int64;
  left: int64;
  right: int64;
end;
type TreeState = record
  nodes: array of Node;
  root: int64;
end;
type NodeArray = array of Node;
type IntArray = array of int64;
type IntArrayArray = array of IntArray;
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
procedure json(x: int64);
begin
  writeln(x);
end;
function list_int_to_str(xs: array of int64): string;
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
function makeTreeState(nodes: NodeArray; root: int64): TreeState; forward;
function makeNode(value: int64; left: int64; right: int64): Node; forward;
function new_node(new_node_state: TreeState; new_node_value: int64): int64; forward;
procedure insert(insert_state: TreeState; insert_value: int64); forward;
function inorder(inorder_state: TreeState; inorder_idx: int64): IntArray; forward;
function tree_sort(tree_sort_arr: IntArray): IntArray; forward;
function makeTreeState(nodes: NodeArray; root: int64): TreeState;
begin
  Result.nodes := nodes;
  Result.root := root;
end;
function makeNode(value: int64; left: int64; right: int64): Node;
begin
  Result.value := value;
  Result.left := left;
  Result.right := right;
end;
function new_node(new_node_state: TreeState; new_node_value: int64): int64;
begin
  new_node_state.nodes := concat(new_node_state.nodes, [makeNode(new_node_value, -1, -1)]);
  exit(Length(new_node_state.nodes) - 1);
end;
procedure insert(insert_state: TreeState; insert_value: int64);
var
  insert_current: int64;
  insert_nodes: array of Node;
  insert_node_var: Node;
  insert_idx: int64;
  insert_idx_8: int64;
begin
  if insert_state.root = -1 then begin
  insert_state.root := new_node(insert_state, insert_value);
  exit();
end;
  insert_current := insert_state.root;
  insert_nodes := insert_state.nodes;
  while true do begin
  insert_node_var := insert_nodes[insert_current];
  if insert_value < insert_node_var.value then begin
  if insert_node_var.left = -1 then begin
  insert_idx := new_node(insert_state, insert_value);
  insert_nodes := insert_state.nodes;
  insert_node_var.left := insert_idx;
  insert_nodes[insert_current] := insert_node_var;
  insert_state.nodes := insert_nodes;
  exit();
end;
  insert_current := insert_node_var.left;
end else begin
  if insert_value > insert_node_var.value then begin
  if insert_node_var.right = -1 then begin
  insert_idx_8 := new_node(insert_state, insert_value);
  insert_nodes := insert_state.nodes;
  insert_node_var.right := insert_idx_8;
  insert_nodes[insert_current] := insert_node_var;
  insert_state.nodes := insert_nodes;
  exit();
end;
  insert_current := insert_node_var.right;
end else begin
  exit();
end;
end;
end;
end;
function inorder(inorder_state: TreeState; inorder_idx: int64): IntArray;
var
  inorder_node_var: Node;
  inorder_result_: IntArray;
  inorder_right_part: array of int64;
  inorder_i: int64;
begin
  if inorder_idx = -1 then begin
  exit([]);
end;
  inorder_node_var := inorder_state.nodes[inorder_idx];
  inorder_result_ := inorder(inorder_state, inorder_node_var.left);
  inorder_result_ := concat(inorder_result_, IntArray([inorder_node_var.value]));
  inorder_right_part := inorder(inorder_state, inorder_node_var.right);
  inorder_i := 0;
  while inorder_i < Length(inorder_right_part) do begin
  inorder_result_ := concat(inorder_result_, IntArray([inorder_right_part[inorder_i]]));
  inorder_i := inorder_i + 1;
end;
  exit(inorder_result_);
end;
function tree_sort(tree_sort_arr: IntArray): IntArray;
var
  tree_sort_state: TreeState;
  tree_sort_i: int64;
begin
  tree_sort_state := makeTreeState([], -1);
  tree_sort_i := 0;
  while tree_sort_i < Length(tree_sort_arr) do begin
  insert(tree_sort_state, tree_sort_arr[tree_sort_i]);
  tree_sort_i := tree_sort_i + 1;
end;
  if tree_sort_state.root = -1 then begin
  exit([]);
end;
  exit(inorder(tree_sort_state, tree_sort_state.root));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(tree_sort([])));
  writeln(list_int_to_str(tree_sort([1])));
  writeln(list_int_to_str(tree_sort([1, 2])));
  writeln(list_int_to_str(tree_sort([5, 2, 7])));
  writeln(list_int_to_str(tree_sort([5, -4, 9, 2, 7])));
  writeln(list_int_to_str(tree_sort([5, 6, 1, -1, 4, 37, 2, 7])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
