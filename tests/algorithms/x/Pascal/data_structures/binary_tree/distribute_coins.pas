{$mode objfpc}
program Main;
uses SysUtils;
type TreeNode = record
  data: integer;
  left: integer;
  right: integer;
end;
type TreeNodeArray = array of TreeNode;
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
  total_moves: integer;
  idx: integer;
  x: integer;
  nodes: TreeNodeArray;
  root: integer;
function makeTreeNode(data: integer; left: integer; right: integer): TreeNode; forward;
function count_nodes(nodes: TreeNodeArray; idx: integer): integer; forward;
function count_coins(nodes: TreeNodeArray; idx: integer): integer; forward;
function iabs(x: integer): integer; forward;
function dfs(nodes: TreeNodeArray; idx: integer): integer; forward;
function distribute_coins(nodes: TreeNodeArray; root: integer): integer; forward;
procedure main(); forward;
function makeTreeNode(data: integer; left: integer; right: integer): TreeNode;
begin
  Result.data := data;
  Result.left := left;
  Result.right := right;
end;
function count_nodes(nodes: TreeNodeArray; idx: integer): integer;
var
  count_nodes_node: TreeNode;
begin
  if idx = 0 then begin
  exit(0);
end;
  count_nodes_node := nodes[idx];
  exit((count_nodes(nodes, count_nodes_node.left) + count_nodes(nodes, count_nodes_node.right)) + 1);
end;
function count_coins(nodes: TreeNodeArray; idx: integer): integer;
var
  count_coins_node: TreeNode;
begin
  if idx = 0 then begin
  exit(0);
end;
  count_coins_node := nodes[idx];
  exit((count_coins(nodes, count_coins_node.left) + count_coins(nodes, count_coins_node.right)) + count_coins_node.data);
end;
function iabs(x: integer): integer;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function dfs(nodes: TreeNodeArray; idx: integer): integer;
var
  dfs_node: TreeNode;
  dfs_left_excess: integer;
  dfs_right_excess: integer;
  dfs_abs_left: integer;
  dfs_abs_right: integer;
begin
  if idx = 0 then begin
  exit(0);
end;
  dfs_node := nodes[idx];
  dfs_left_excess := dfs(nodes, dfs_node.left);
  dfs_right_excess := dfs(nodes, dfs_node.right);
  dfs_abs_left := iabs(dfs_left_excess);
  dfs_abs_right := iabs(dfs_right_excess);
  total_moves := (total_moves + dfs_abs_left) + dfs_abs_right;
  exit(((dfs_node.data + dfs_left_excess) + dfs_right_excess) - 1);
end;
function distribute_coins(nodes: TreeNodeArray; root: integer): integer;
begin
  if root = 0 then begin
  exit(0);
end;
  if count_nodes(nodes, root) <> count_coins(nodes, root) then begin
  panic('The nodes number should be same as the number of coins');
end;
  total_moves := 0;
  dfs(nodes, root);
  exit(total_moves);
end;
procedure main();
var
  main_example1: array of TreeNode;
  main_example2: array of TreeNode;
  main_example3: array of TreeNode;
begin
  main_example1 := [makeTreeNode(0, 0, 0), makeTreeNode(3, 2, 3), makeTreeNode(0, 0, 0), makeTreeNode(0, 0, 0)];
  main_example2 := [makeTreeNode(0, 0, 0), makeTreeNode(0, 2, 3), makeTreeNode(3, 0, 0), makeTreeNode(0, 0, 0)];
  main_example3 := [makeTreeNode(0, 0, 0), makeTreeNode(0, 2, 3), makeTreeNode(0, 0, 0), makeTreeNode(3, 0, 0)];
  writeln(distribute_coins(main_example1, 1));
  writeln(distribute_coins(main_example2, 1));
  writeln(distribute_coins(main_example3, 1));
  writeln(distribute_coins([makeTreeNode(0, 0, 0)], 0));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  total_moves := 0;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
