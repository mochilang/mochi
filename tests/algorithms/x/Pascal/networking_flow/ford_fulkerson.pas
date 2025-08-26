{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
type BoolArray = array of boolean;
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
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  INF: int64;
  graph: array of IntArray;
function breadth_first_search(breadth_first_search_graph: IntArrayArray; breadth_first_search_source: int64; breadth_first_search_sink: int64; breadth_first_search_parent: IntArray): boolean; forward;
function ford_fulkerson(ford_fulkerson_graph: IntArrayArray; ford_fulkerson_source: int64; ford_fulkerson_sink: int64): int64; forward;
function breadth_first_search(breadth_first_search_graph: IntArrayArray; breadth_first_search_source: int64; breadth_first_search_sink: int64; breadth_first_search_parent: IntArray): boolean;
var
  breadth_first_search_visited: array of boolean;
  breadth_first_search_i: int64;
  breadth_first_search_queue: array of int64;
  breadth_first_search_head: int64;
  breadth_first_search_u: int64;
  breadth_first_search_row: array of int64;
  breadth_first_search_ind: int64;
  breadth_first_search_capacity: int64;
begin
  breadth_first_search_visited := [];
  breadth_first_search_i := 0;
  while breadth_first_search_i < Length(breadth_first_search_graph) do begin
  breadth_first_search_visited := concat(breadth_first_search_visited, [false]);
  breadth_first_search_i := breadth_first_search_i + 1;
end;
  breadth_first_search_queue := [];
  breadth_first_search_queue := concat(breadth_first_search_queue, IntArray([breadth_first_search_source]));
  breadth_first_search_visited[breadth_first_search_source] := true;
  breadth_first_search_head := 0;
  while breadth_first_search_head < Length(breadth_first_search_queue) do begin
  breadth_first_search_u := breadth_first_search_queue[breadth_first_search_head];
  breadth_first_search_head := breadth_first_search_head + 1;
  breadth_first_search_row := breadth_first_search_graph[breadth_first_search_u];
  breadth_first_search_ind := 0;
  while breadth_first_search_ind < Length(breadth_first_search_row) do begin
  breadth_first_search_capacity := breadth_first_search_row[breadth_first_search_ind];
  if (breadth_first_search_visited[breadth_first_search_ind] = false) and (breadth_first_search_capacity > 0) then begin
  breadth_first_search_queue := concat(breadth_first_search_queue, IntArray([breadth_first_search_ind]));
  breadth_first_search_visited[breadth_first_search_ind] := true;
  breadth_first_search_parent[breadth_first_search_ind] := breadth_first_search_u;
end;
  breadth_first_search_ind := breadth_first_search_ind + 1;
end;
end;
  exit(breadth_first_search_visited[breadth_first_search_sink]);
end;
function ford_fulkerson(ford_fulkerson_graph: IntArrayArray; ford_fulkerson_source: int64; ford_fulkerson_sink: int64): int64;
var
  ford_fulkerson_parent: array of int64;
  ford_fulkerson_i: int64;
  ford_fulkerson_max_flow: int64;
  ford_fulkerson_path_flow: int64;
  ford_fulkerson_s: int64;
  ford_fulkerson_prev: int64;
  ford_fulkerson_cap: int64;
  ford_fulkerson_v: int64;
  ford_fulkerson_u: int64;
  ford_fulkerson_j: int64;
begin
  ford_fulkerson_parent := [];
  ford_fulkerson_i := 0;
  while ford_fulkerson_i < Length(ford_fulkerson_graph) do begin
  ford_fulkerson_parent := concat(ford_fulkerson_parent, IntArray([-1]));
  ford_fulkerson_i := ford_fulkerson_i + 1;
end;
  ford_fulkerson_max_flow := 0;
  while breadth_first_search(ford_fulkerson_graph, ford_fulkerson_source, ford_fulkerson_sink, ford_fulkerson_parent) do begin
  ford_fulkerson_path_flow := INF;
  ford_fulkerson_s := ford_fulkerson_sink;
  while ford_fulkerson_s <> ford_fulkerson_source do begin
  ford_fulkerson_prev := ford_fulkerson_parent[ford_fulkerson_s];
  ford_fulkerson_cap := ford_fulkerson_graph[ford_fulkerson_prev][ford_fulkerson_s];
  if ford_fulkerson_cap < ford_fulkerson_path_flow then begin
  ford_fulkerson_path_flow := ford_fulkerson_cap;
end;
  ford_fulkerson_s := ford_fulkerson_prev;
end;
  ford_fulkerson_max_flow := ford_fulkerson_max_flow + ford_fulkerson_path_flow;
  ford_fulkerson_v := ford_fulkerson_sink;
  while ford_fulkerson_v <> ford_fulkerson_source do begin
  ford_fulkerson_u := ford_fulkerson_parent[ford_fulkerson_v];
  ford_fulkerson_graph[ford_fulkerson_u][ford_fulkerson_v] := ford_fulkerson_graph[ford_fulkerson_u][ford_fulkerson_v] - ford_fulkerson_path_flow;
  ford_fulkerson_graph[ford_fulkerson_v][ford_fulkerson_u] := ford_fulkerson_graph[ford_fulkerson_v][ford_fulkerson_u] + ford_fulkerson_path_flow;
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
  writeln('');
end.
