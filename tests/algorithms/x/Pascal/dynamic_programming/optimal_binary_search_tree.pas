{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type Node = record
  key: int64;
  freq: int64;
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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function makeNode(key: int64; freq: int64): Node; forward;
function sort_nodes(sort_nodes_nodes: NodeArray): NodeArray; forward;
procedure print_node(print_node_n: Node); forward;
procedure print_binary_search_tree(print_binary_search_tree_root: IntArrayArray; print_binary_search_tree_keys: IntArray; print_binary_search_tree_i: int64; print_binary_search_tree_j: int64; print_binary_search_tree_parent: int64; print_binary_search_tree_is_left: boolean); forward;
procedure find_optimal_binary_search_tree(find_optimal_binary_search_tree_original_nodes: NodeArray); forward;
procedure main(); forward;
function makeNode(key: int64; freq: int64): Node;
begin
  Result.key := key;
  Result.freq := freq;
end;
function sort_nodes(sort_nodes_nodes: NodeArray): NodeArray;
var
  sort_nodes_arr: array of Node;
  sort_nodes_i: int64;
  sort_nodes_key_node: Node;
  sort_nodes_j: int64;
  sort_nodes_temp: Node;
begin
  sort_nodes_arr := sort_nodes_nodes;
  sort_nodes_i := 1;
  while sort_nodes_i < Length(sort_nodes_arr) do begin
  sort_nodes_key_node := sort_nodes_arr[sort_nodes_i];
  sort_nodes_j := sort_nodes_i - 1;
  while sort_nodes_j >= 0 do begin
  sort_nodes_temp := sort_nodes_arr[sort_nodes_j];
  if sort_nodes_temp.key > sort_nodes_key_node.key then begin
  sort_nodes_arr[sort_nodes_j + 1] := sort_nodes_temp;
  sort_nodes_j := sort_nodes_j - 1;
end else begin
  break;
end;
end;
  sort_nodes_arr[sort_nodes_j + 1] := sort_nodes_key_node;
  sort_nodes_i := sort_nodes_i + 1;
end;
  exit(sort_nodes_arr);
end;
procedure print_node(print_node_n: Node);
begin
  writeln(((('Node(key=' + IntToStr(print_node_n.key)) + ', freq=') + IntToStr(print_node_n.freq)) + ')');
end;
procedure print_binary_search_tree(print_binary_search_tree_root: IntArrayArray; print_binary_search_tree_keys: IntArray; print_binary_search_tree_i: int64; print_binary_search_tree_j: int64; print_binary_search_tree_parent: int64; print_binary_search_tree_is_left: boolean);
var
  print_binary_search_tree_node_var: int64;
begin
  if ((print_binary_search_tree_i > print_binary_search_tree_j) or (print_binary_search_tree_i < 0)) or (print_binary_search_tree_j > (Length(print_binary_search_tree_root) - 1)) then begin
  exit();
end;
  print_binary_search_tree_node_var := print_binary_search_tree_root[print_binary_search_tree_i][print_binary_search_tree_j];
  if print_binary_search_tree_parent = -1 then begin
  writeln(IntToStr(print_binary_search_tree_keys[print_binary_search_tree_node_var]) + ' is the root of the binary search tree.');
end else begin
  if print_binary_search_tree_is_left then begin
  writeln(((IntToStr(print_binary_search_tree_keys[print_binary_search_tree_node_var]) + ' is the left child of key ') + IntToStr(print_binary_search_tree_parent)) + '.');
end else begin
  writeln(((IntToStr(print_binary_search_tree_keys[print_binary_search_tree_node_var]) + ' is the right child of key ') + IntToStr(print_binary_search_tree_parent)) + '.');
end;
end;
  print_binary_search_tree(print_binary_search_tree_root, print_binary_search_tree_keys, print_binary_search_tree_i, print_binary_search_tree_node_var - 1, print_binary_search_tree_keys[print_binary_search_tree_node_var], true);
  print_binary_search_tree(print_binary_search_tree_root, print_binary_search_tree_keys, print_binary_search_tree_node_var + 1, print_binary_search_tree_j, print_binary_search_tree_keys[print_binary_search_tree_node_var], false);
end;
procedure find_optimal_binary_search_tree(find_optimal_binary_search_tree_original_nodes: NodeArray);
var
  find_optimal_binary_search_tree_nodes: array of Node;
  find_optimal_binary_search_tree_n: integer;
  find_optimal_binary_search_tree_keys: array of int64;
  find_optimal_binary_search_tree_freqs: array of int64;
  find_optimal_binary_search_tree_i: int64;
  find_optimal_binary_search_tree_node_var: Node;
  find_optimal_binary_search_tree_dp: array of IntArray;
  find_optimal_binary_search_tree_total: array of IntArray;
  find_optimal_binary_search_tree_root: array of IntArray;
  find_optimal_binary_search_tree_dp_row: array of int64;
  find_optimal_binary_search_tree_total_row: array of int64;
  find_optimal_binary_search_tree_root_row: array of int64;
  find_optimal_binary_search_tree_j: int64;
  find_optimal_binary_search_tree_interval_length: int64;
  find_optimal_binary_search_tree_INF: int64;
  find_optimal_binary_search_tree_r: int64;
  find_optimal_binary_search_tree_left: int64;
  find_optimal_binary_search_tree_right: int64;
  find_optimal_binary_search_tree_cost: int64;
begin
  find_optimal_binary_search_tree_nodes := sort_nodes(find_optimal_binary_search_tree_original_nodes);
  find_optimal_binary_search_tree_n := Length(find_optimal_binary_search_tree_nodes);
  find_optimal_binary_search_tree_keys := [];
  find_optimal_binary_search_tree_freqs := [];
  find_optimal_binary_search_tree_i := 0;
  while find_optimal_binary_search_tree_i < find_optimal_binary_search_tree_n do begin
  find_optimal_binary_search_tree_node_var := find_optimal_binary_search_tree_nodes[find_optimal_binary_search_tree_i];
  find_optimal_binary_search_tree_keys := concat(find_optimal_binary_search_tree_keys, IntArray([find_optimal_binary_search_tree_node_var.key]));
  find_optimal_binary_search_tree_freqs := concat(find_optimal_binary_search_tree_freqs, IntArray([find_optimal_binary_search_tree_node_var.freq]));
  find_optimal_binary_search_tree_i := find_optimal_binary_search_tree_i + 1;
end;
  find_optimal_binary_search_tree_dp := [];
  find_optimal_binary_search_tree_total := [];
  find_optimal_binary_search_tree_root := [];
  find_optimal_binary_search_tree_i := 0;
  while find_optimal_binary_search_tree_i < find_optimal_binary_search_tree_n do begin
  find_optimal_binary_search_tree_dp_row := [];
  find_optimal_binary_search_tree_total_row := [];
  find_optimal_binary_search_tree_root_row := [];
  find_optimal_binary_search_tree_j := 0;
  while find_optimal_binary_search_tree_j < find_optimal_binary_search_tree_n do begin
  if find_optimal_binary_search_tree_i = find_optimal_binary_search_tree_j then begin
  find_optimal_binary_search_tree_dp_row := concat(find_optimal_binary_search_tree_dp_row, IntArray([find_optimal_binary_search_tree_freqs[find_optimal_binary_search_tree_i]]));
  find_optimal_binary_search_tree_total_row := concat(find_optimal_binary_search_tree_total_row, IntArray([find_optimal_binary_search_tree_freqs[find_optimal_binary_search_tree_i]]));
  find_optimal_binary_search_tree_root_row := concat(find_optimal_binary_search_tree_root_row, IntArray([find_optimal_binary_search_tree_i]));
end else begin
  find_optimal_binary_search_tree_dp_row := concat(find_optimal_binary_search_tree_dp_row, IntArray([0]));
  find_optimal_binary_search_tree_total_row := concat(find_optimal_binary_search_tree_total_row, IntArray([0]));
  find_optimal_binary_search_tree_root_row := concat(find_optimal_binary_search_tree_root_row, IntArray([0]));
end;
  find_optimal_binary_search_tree_j := find_optimal_binary_search_tree_j + 1;
end;
  find_optimal_binary_search_tree_dp := concat(find_optimal_binary_search_tree_dp, [find_optimal_binary_search_tree_dp_row]);
  find_optimal_binary_search_tree_total := concat(find_optimal_binary_search_tree_total, [find_optimal_binary_search_tree_total_row]);
  find_optimal_binary_search_tree_root := concat(find_optimal_binary_search_tree_root, [find_optimal_binary_search_tree_root_row]);
  find_optimal_binary_search_tree_i := find_optimal_binary_search_tree_i + 1;
end;
  find_optimal_binary_search_tree_interval_length := 2;
  find_optimal_binary_search_tree_INF := 2147483647;
  while find_optimal_binary_search_tree_interval_length <= find_optimal_binary_search_tree_n do begin
  find_optimal_binary_search_tree_i := 0;
  while find_optimal_binary_search_tree_i < ((find_optimal_binary_search_tree_n - find_optimal_binary_search_tree_interval_length) + 1) do begin
  find_optimal_binary_search_tree_j := (find_optimal_binary_search_tree_i + find_optimal_binary_search_tree_interval_length) - 1;
  find_optimal_binary_search_tree_dp[find_optimal_binary_search_tree_i][find_optimal_binary_search_tree_j] := find_optimal_binary_search_tree_INF;
  find_optimal_binary_search_tree_total[find_optimal_binary_search_tree_i][find_optimal_binary_search_tree_j] := find_optimal_binary_search_tree_total[find_optimal_binary_search_tree_i][find_optimal_binary_search_tree_j - 1] + find_optimal_binary_search_tree_freqs[find_optimal_binary_search_tree_j];
  find_optimal_binary_search_tree_r := find_optimal_binary_search_tree_root[find_optimal_binary_search_tree_i][find_optimal_binary_search_tree_j - 1];
  while find_optimal_binary_search_tree_r <= find_optimal_binary_search_tree_root[find_optimal_binary_search_tree_i + 1][find_optimal_binary_search_tree_j] do begin
  if find_optimal_binary_search_tree_r <> find_optimal_binary_search_tree_i then begin
  find_optimal_binary_search_tree_left := find_optimal_binary_search_tree_dp[find_optimal_binary_search_tree_i][find_optimal_binary_search_tree_r - 1];
end else begin
  find_optimal_binary_search_tree_left := 0;
end;
  if find_optimal_binary_search_tree_r <> find_optimal_binary_search_tree_j then begin
  find_optimal_binary_search_tree_right := find_optimal_binary_search_tree_dp[find_optimal_binary_search_tree_r + 1][find_optimal_binary_search_tree_j];
end else begin
  find_optimal_binary_search_tree_right := 0;
end;
  find_optimal_binary_search_tree_cost := (find_optimal_binary_search_tree_left + find_optimal_binary_search_tree_total[find_optimal_binary_search_tree_i][find_optimal_binary_search_tree_j]) + find_optimal_binary_search_tree_right;
  if find_optimal_binary_search_tree_dp[find_optimal_binary_search_tree_i][find_optimal_binary_search_tree_j] > find_optimal_binary_search_tree_cost then begin
  find_optimal_binary_search_tree_dp[find_optimal_binary_search_tree_i][find_optimal_binary_search_tree_j] := find_optimal_binary_search_tree_cost;
  find_optimal_binary_search_tree_root[find_optimal_binary_search_tree_i][find_optimal_binary_search_tree_j] := find_optimal_binary_search_tree_r;
end;
  find_optimal_binary_search_tree_r := find_optimal_binary_search_tree_r + 1;
end;
  find_optimal_binary_search_tree_i := find_optimal_binary_search_tree_i + 1;
end;
  find_optimal_binary_search_tree_interval_length := find_optimal_binary_search_tree_interval_length + 1;
end;
  writeln('Binary search tree nodes:');
  find_optimal_binary_search_tree_i := 0;
  while find_optimal_binary_search_tree_i < find_optimal_binary_search_tree_n do begin
  print_node(find_optimal_binary_search_tree_nodes[find_optimal_binary_search_tree_i]);
  find_optimal_binary_search_tree_i := find_optimal_binary_search_tree_i + 1;
end;
  writeln((#10 + 'The cost of optimal BST for given tree nodes is ' + IntToStr(find_optimal_binary_search_tree_dp[0][find_optimal_binary_search_tree_n - 1])) + '.');
  print_binary_search_tree(find_optimal_binary_search_tree_root, find_optimal_binary_search_tree_keys, 0, find_optimal_binary_search_tree_n - 1, -1, false);
end;
procedure main();
var
  main_nodes: array of Node;
begin
  main_nodes := [makeNode(12, 8), makeNode(10, 34), makeNode(20, 50), makeNode(42, 3), makeNode(25, 40), makeNode(37, 30)];
  find_optimal_binary_search_tree(main_nodes);
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
  writeln('');
end.
