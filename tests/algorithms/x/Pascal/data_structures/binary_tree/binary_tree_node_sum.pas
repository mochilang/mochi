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
  example: array of Node;
  index: integer;
  tree: NodeArray;
function makeNode(value: integer; left: integer; right: integer): Node; forward;
function node_sum(tree: NodeArray; index: integer): integer; forward;
function makeNode(value: integer; left: integer; right: integer): Node;
begin
  Result.value := value;
  Result.left := left;
  Result.right := right;
end;
function node_sum(tree: NodeArray; index: integer): integer;
var
  node_sum_node_var: Node;
begin
  if index = -1 then begin
  exit(0);
end;
  node_sum_node_var := tree[index];
  exit((node_sum_node_var.value + node_sum(tree, node_sum_node_var.left)) + node_sum(tree, node_sum_node_var.right));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  example := [makeNode(10, 1, 2), makeNode(5, 3, -1), makeNode(-3, 4, 5), makeNode(12, -1, -1), makeNode(8, -1, -1), makeNode(0, -1, -1)];
  writeln(node_sum(example, 0));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
