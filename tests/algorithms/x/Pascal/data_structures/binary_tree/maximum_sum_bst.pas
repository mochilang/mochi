{$mode objfpc}
program Main;
uses SysUtils;
type Node = record
  val: integer;
  left: integer;
  right: integer;
end;
type Info = record
  is_bst: boolean;
  min_val: integer;
  max_val: integer;
  total: integer;
  best: integer;
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
  a: integer;
  b: integer;
  root: integer;
  nodes: NodeArray;
function makeInfo(is_bst: boolean; min_val: integer; max_val: integer; total: integer; best: integer): Info; forward;
function makeNode(val: integer; left: integer; right: integer): Node; forward;
function min_int(a: integer; b: integer): integer; forward;
function max_int(a: integer; b: integer): integer; forward;
function solver(nodes: NodeArray; idx: integer): Info; forward;
function max_sum_bst(nodes: NodeArray; root: integer): integer; forward;
procedure main(); forward;
function makeInfo(is_bst: boolean; min_val: integer; max_val: integer; total: integer; best: integer): Info;
begin
  Result.is_bst := is_bst;
  Result.min_val := min_val;
  Result.max_val := max_val;
  Result.total := total;
  Result.best := best;
end;
function makeNode(val: integer; left: integer; right: integer): Node;
begin
  Result.val := val;
  Result.left := left;
  Result.right := right;
end;
function min_int(a: integer; b: integer): integer;
begin
  if a < b then begin
  exit(a);
end;
  exit(b);
end;
function max_int(a: integer; b: integer): integer;
begin
  if a > b then begin
  exit(a);
end;
  exit(b);
end;
function solver(nodes: NodeArray; idx: integer): Info;
var
  solver_node_var: Node;
  solver_left_info: Info;
  solver_right_info: Info;
  solver_current_best: integer;
  solver_sum_val: integer;
begin
  if idx = (0 - 1) then begin
  exit(makeInfo(true, 2147483647, -2147483648, 0, 0));
end;
  solver_node_var := nodes[idx];
  solver_left_info := solver(nodes, solver_node_var.left);
  solver_right_info := solver(nodes, solver_node_var.right);
  solver_current_best := max_int(solver_left_info.best, solver_right_info.best);
  if ((solver_left_info.is_bst and solver_right_info.is_bst) and (solver_left_info.max_val < solver_node_var.val)) and (solver_node_var.val < solver_right_info.min_val) then begin
  solver_sum_val := (solver_left_info.total + solver_right_info.total) + solver_node_var.val;
  solver_current_best := max_int(solver_current_best, solver_sum_val);
  exit(makeInfo(true, min_int(solver_left_info.min_val, solver_node_var.val), max_int(solver_right_info.max_val, solver_node_var.val), solver_sum_val, solver_current_best));
end;
  exit(makeInfo(false, 0, 0, 0, solver_current_best));
end;
function max_sum_bst(nodes: NodeArray; root: integer): integer;
var
  max_sum_bst_info_var: Info;
begin
  max_sum_bst_info_var := solver(nodes, root);
  exit(max_sum_bst_info_var.best);
end;
procedure main();
var
  main_t1_nodes: array of Node;
  main_t2_nodes: array of Node;
  main_t3_nodes: array of Node;
begin
  main_t1_nodes := [makeNode(4, 1, 0 - 1), makeNode(3, 2, 3), makeNode(1, 0 - 1, 0 - 1), makeNode(2, 0 - 1, 0 - 1)];
  writeln(max_sum_bst(main_t1_nodes, 0));
  main_t2_nodes := [makeNode(-4, 1, 2), makeNode(-2, 0 - 1, 0 - 1), makeNode(-5, 0 - 1, 0 - 1)];
  writeln(max_sum_bst(main_t2_nodes, 0));
  main_t3_nodes := [makeNode(1, 1, 2), makeNode(4, 3, 4), makeNode(3, 5, 6), makeNode(2, 0 - 1, 0 - 1), makeNode(4, 0 - 1, 0 - 1), makeNode(2, 0 - 1, 0 - 1), makeNode(5, 7, 8), makeNode(4, 0 - 1, 0 - 1), makeNode(6, 0 - 1, 0 - 1)];
  writeln(max_sum_bst(main_t3_nodes, 0));
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
