{$mode objfpc}
program Main;
uses SysUtils;
type Node = record
  value: integer;
  left: integer;
  right: integer;
end;
type NodeArray = array of Node;
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
  idx: integer;
  nodes: NodeArray;
function makeNode(value: integer; left: integer; right: integer): Node; forward;
function tree_sum(nodes: NodeArray; idx: integer): integer; forward;
function is_sum_node(nodes: NodeArray; idx: integer): boolean; forward;
function build_a_tree(): NodeArray; forward;
function build_a_sum_tree(): NodeArray; forward;
function makeNode(value: integer; left: integer; right: integer): Node;
begin
  Result.value := value;
  Result.left := left;
  Result.right := right;
end;
function tree_sum(nodes: NodeArray; idx: integer): integer;
var
  tree_sum_node_var: Node;
begin
  if idx = -1 then begin
  exit(0);
end;
  tree_sum_node_var := nodes[idx];
  exit((tree_sum_node_var.value + tree_sum(nodes, tree_sum_node_var.left)) + tree_sum(nodes, tree_sum_node_var.right));
end;
function is_sum_node(nodes: NodeArray; idx: integer): boolean;
var
  is_sum_node_node_var: Node;
  is_sum_node_left_sum: integer;
  is_sum_node_right_sum: integer;
  is_sum_node_left_ok: boolean;
  is_sum_node_right_ok: boolean;
begin
  is_sum_node_node_var := nodes[idx];
  if (is_sum_node_node_var.left = -1) and (is_sum_node_node_var.right = -1) then begin
  exit(true);
end;
  is_sum_node_left_sum := tree_sum(nodes, is_sum_node_node_var.left);
  is_sum_node_right_sum := tree_sum(nodes, is_sum_node_node_var.right);
  if is_sum_node_node_var.value <> (is_sum_node_left_sum + is_sum_node_right_sum) then begin
  exit(false);
end;
  is_sum_node_left_ok := true;
  if is_sum_node_node_var.left <> -1 then begin
  is_sum_node_left_ok := is_sum_node(nodes, is_sum_node_node_var.left);
end;
  is_sum_node_right_ok := true;
  if is_sum_node_node_var.right <> -1 then begin
  is_sum_node_right_ok := is_sum_node(nodes, is_sum_node_node_var.right);
end;
  exit(is_sum_node_left_ok and is_sum_node_right_ok);
end;
function build_a_tree(): NodeArray;
begin
  exit([makeNode(11, 1, 2), makeNode(2, 3, 4), makeNode(29, 5, 6), makeNode(1, -1, -1), makeNode(7, -1, -1), makeNode(15, -1, -1), makeNode(40, 7, -1), makeNode(35, -1, -1)]);
end;
function build_a_sum_tree(): NodeArray;
begin
  exit([makeNode(26, 1, 2), makeNode(10, 3, 4), makeNode(3, -1, 5), makeNode(4, -1, -1), makeNode(6, -1, -1), makeNode(3, -1, -1)]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
