{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
type Graph = record
  n: integer;
  dp: array of IntArray;
end;
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
  INF: integer;
  graph_var: Graph;
  v: integer;
  g: Graph;
  u: integer;
  w: integer;
  n: integer;
function makeGraph(n: integer; dp: IntArrayArray): Graph; forward;
function new_graph(n: integer): Graph; forward;
procedure add_edge(g: Graph; u: integer; v: integer; w: integer); forward;
procedure floyd_warshall(g: Graph); forward;
function show_min(g: Graph; u: integer; v: integer): integer; forward;
function makeGraph(n: integer; dp: IntArrayArray): Graph;
begin
  Result.n := n;
  Result.dp := dp;
end;
function new_graph(n: integer): Graph;
var
  new_graph_dp: array of IntArray;
  new_graph_i: integer;
  new_graph_row: array of integer;
  new_graph_j: integer;
begin
  new_graph_dp := [];
  new_graph_i := 0;
  while new_graph_i < n do begin
  new_graph_row := [];
  new_graph_j := 0;
  while new_graph_j < n do begin
  if new_graph_i = new_graph_j then begin
  new_graph_row := concat(new_graph_row, IntArray([0]));
end else begin
  new_graph_row := concat(new_graph_row, IntArray([INF]));
end;
  new_graph_j := new_graph_j + 1;
end;
  new_graph_dp := concat(new_graph_dp, [new_graph_row]);
  new_graph_i := new_graph_i + 1;
end;
  exit(makeGraph(n, new_graph_dp));
end;
procedure add_edge(g: Graph; u: integer; v: integer; w: integer);
var
  add_edge_dp: array of IntArray;
  add_edge_row: array of integer;
begin
  add_edge_dp := g.dp;
  add_edge_row := add_edge_dp[u];
  add_edge_row[v] := w;
  add_edge_dp[u] := add_edge_row;
  g.dp := add_edge_dp;
end;
procedure floyd_warshall(g: Graph);
var
  floyd_warshall_dp: array of IntArray;
  floyd_warshall_k: integer;
  floyd_warshall_i: integer;
  floyd_warshall_j: integer;
  floyd_warshall_alt: integer;
  floyd_warshall_row: array of integer;
begin
  floyd_warshall_dp := g.dp;
  floyd_warshall_k := 0;
  while floyd_warshall_k < g.n do begin
  floyd_warshall_i := 0;
  while floyd_warshall_i < g.n do begin
  floyd_warshall_j := 0;
  while floyd_warshall_j < g.n do begin
  floyd_warshall_alt := floyd_warshall_dp[floyd_warshall_i][floyd_warshall_k] + floyd_warshall_dp[floyd_warshall_k][floyd_warshall_j];
  floyd_warshall_row := floyd_warshall_dp[floyd_warshall_i];
  if floyd_warshall_alt < floyd_warshall_row[floyd_warshall_j] then begin
  floyd_warshall_row[floyd_warshall_j] := floyd_warshall_alt;
  floyd_warshall_dp[floyd_warshall_i] := floyd_warshall_row;
end;
  floyd_warshall_j := floyd_warshall_j + 1;
end;
  floyd_warshall_i := floyd_warshall_i + 1;
end;
  floyd_warshall_k := floyd_warshall_k + 1;
end;
  g.dp := floyd_warshall_dp;
end;
function show_min(g: Graph; u: integer; v: integer): integer;
begin
  exit(g.dp[u][v]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  INF := 1000000000;
  graph_var := new_graph(5);
  add_edge(graph_var, 0, 2, 9);
  add_edge(graph_var, 0, 4, 10);
  add_edge(graph_var, 1, 3, 5);
  add_edge(graph_var, 2, 3, 7);
  add_edge(graph_var, 3, 0, 10);
  add_edge(graph_var, 3, 1, 2);
  add_edge(graph_var, 3, 2, 1);
  add_edge(graph_var, 3, 4, 6);
  add_edge(graph_var, 4, 1, 3);
  add_edge(graph_var, 4, 2, 4);
  add_edge(graph_var, 4, 3, 9);
  floyd_warshall(graph_var);
  writeln(IntToStr(show_min(graph_var, 1, 4)));
  writeln(IntToStr(show_min(graph_var, 0, 3)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
