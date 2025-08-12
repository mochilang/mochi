{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type BoolArray = array of boolean;
type IntArray = array of integer;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  INF: integer;
  graph: array of IntArray;
  parent: IntArray;
  sink: integer;
  source: integer;
function breadth_first_search(graph: IntArrayArray; source: integer; sink: integer; parent: IntArray): boolean; forward;
function ford_fulkerson(graph: IntArrayArray; source: integer; sink: integer): integer; forward;
function breadth_first_search(graph: IntArrayArray; source: integer; sink: integer; parent: IntArray): boolean;
var
  breadth_first_search_visited: array of boolean;
  breadth_first_search_i: integer;
  breadth_first_search_queue: array of integer;
  breadth_first_search_head: integer;
  breadth_first_search_u: integer;
  breadth_first_search_row: array of integer;
  breadth_first_search_ind: integer;
  breadth_first_search_capacity: integer;
begin
  breadth_first_search_visited := [];
  breadth_first_search_i := 0;
  while breadth_first_search_i < Length(graph) do begin
  breadth_first_search_visited := concat(breadth_first_search_visited, [false]);
  breadth_first_search_i := breadth_first_search_i + 1;
end;
  breadth_first_search_queue := [];
  breadth_first_search_queue := concat(breadth_first_search_queue, IntArray([source]));
  breadth_first_search_visited[source] := true;
  breadth_first_search_head := 0;
  while breadth_first_search_head < Length(breadth_first_search_queue) do begin
  breadth_first_search_u := breadth_first_search_queue[breadth_first_search_head];
  breadth_first_search_head := breadth_first_search_head + 1;
  breadth_first_search_row := graph[breadth_first_search_u];
  breadth_first_search_ind := 0;
  while breadth_first_search_ind < Length(breadth_first_search_row) do begin
  breadth_first_search_capacity := breadth_first_search_row[breadth_first_search_ind];
  if (breadth_first_search_visited[breadth_first_search_ind] = false) and (breadth_first_search_capacity > 0) then begin
  breadth_first_search_queue := concat(breadth_first_search_queue, IntArray([breadth_first_search_ind]));
  breadth_first_search_visited[breadth_first_search_ind] := true;
  parent[breadth_first_search_ind] := breadth_first_search_u;
end;
  breadth_first_search_ind := breadth_first_search_ind + 1;
end;
end;
  exit(breadth_first_search_visited[sink]);
end;
function ford_fulkerson(graph: IntArrayArray; source: integer; sink: integer): integer;
var
  ford_fulkerson_parent: array of integer;
  ford_fulkerson_i: integer;
  ford_fulkerson_max_flow: integer;
  ford_fulkerson_path_flow: integer;
  ford_fulkerson_s: integer;
  ford_fulkerson_prev: integer;
  ford_fulkerson_cap: integer;
  ford_fulkerson_v: integer;
  ford_fulkerson_u: integer;
  ford_fulkerson_j: integer;
begin
  ford_fulkerson_parent := [];
  ford_fulkerson_i := 0;
  while ford_fulkerson_i < Length(graph) do begin
  ford_fulkerson_parent := concat(ford_fulkerson_parent, IntArray([-1]));
  ford_fulkerson_i := ford_fulkerson_i + 1;
end;
  ford_fulkerson_max_flow := 0;
  while breadth_first_search(graph, source, sink, ford_fulkerson_parent) do begin
  ford_fulkerson_path_flow := INF;
  ford_fulkerson_s := sink;
  while ford_fulkerson_s <> source do begin
  ford_fulkerson_prev := ford_fulkerson_parent[ford_fulkerson_s];
  ford_fulkerson_cap := graph[ford_fulkerson_prev][ford_fulkerson_s];
  if ford_fulkerson_cap < ford_fulkerson_path_flow then begin
  ford_fulkerson_path_flow := ford_fulkerson_cap;
end;
  ford_fulkerson_s := ford_fulkerson_prev;
end;
  ford_fulkerson_max_flow := ford_fulkerson_max_flow + ford_fulkerson_path_flow;
  ford_fulkerson_v := sink;
  while ford_fulkerson_v <> source do begin
  ford_fulkerson_u := ford_fulkerson_parent[ford_fulkerson_v];
  graph[ford_fulkerson_u][ford_fulkerson_v] := graph[ford_fulkerson_u][ford_fulkerson_v] - ford_fulkerson_path_flow;
  graph[ford_fulkerson_v][ford_fulkerson_u] := graph[ford_fulkerson_v][ford_fulkerson_u] + ford_fulkerson_path_flow;
  ford_fulkerson_v := ford_fulkerson_u;
end;
  ford_fulkerson_j := 0;
  while ford_fulkerson_j < Length(ford_fulkerson_parent) do begin
  ford_fulkerson_parent[ford_fulkerson_j] := -1;
  ford_fulkerson_j := ford_fulkerson_j + 1;
end;
end;
  exit(ford_fulkerson_max_flow);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  INF := 1000000000;
  graph := [[0, 16, 13, 0, 0, 0], [0, 0, 10, 12, 0, 0], [0, 4, 0, 0, 14, 0], [0, 0, 9, 0, 0, 20], [0, 0, 0, 7, 0, 4], [0, 0, 0, 0, 0, 0]];
  writeln(IntToStr(ford_fulkerson(graph, 0, 5)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
