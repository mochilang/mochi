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
  test_graph: array of IntArray;
  result_: IntArrayArray;
function bfs(bfs_graph: IntArrayArray; bfs_s: int64; bfs_t: int64; bfs_parent: IntArray): boolean; forward;
function mincut(mincut_graph: IntArrayArray; mincut_source: int64; mincut_sink: int64): IntArrayArray; forward;
function bfs(bfs_graph: IntArrayArray; bfs_s: int64; bfs_t: int64; bfs_parent: IntArray): boolean;
var
  bfs_visited: array of boolean;
  bfs_i: int64;
  bfs_queue: array of int64;
  bfs_head: int64;
  bfs_u: int64;
  bfs_ind: int64;
begin
  bfs_visited := [];
  bfs_i := 0;
  while bfs_i < Length(bfs_graph) do begin
  bfs_visited := concat(bfs_visited, [false]);
  bfs_i := bfs_i + 1;
end;
  bfs_queue := [bfs_s];
  bfs_head := 0;
  bfs_visited[bfs_s] := true;
  while bfs_head < Length(bfs_queue) do begin
  bfs_u := bfs_queue[bfs_head];
  bfs_head := bfs_head + 1;
  bfs_ind := 0;
  while bfs_ind < Length(bfs_graph[bfs_u]) do begin
  if (bfs_visited[bfs_ind] = false) and (bfs_graph[bfs_u][bfs_ind] > 0) then begin
  bfs_queue := concat(bfs_queue, IntArray([bfs_ind]));
  bfs_visited[bfs_ind] := true;
  bfs_parent[bfs_ind] := bfs_u;
end;
  bfs_ind := bfs_ind + 1;
end;
end;
  exit(bfs_visited[bfs_t]);
end;
function mincut(mincut_graph: IntArrayArray; mincut_source: int64; mincut_sink: int64): IntArrayArray;
var
  mincut_g: array of IntArray;
  mincut_parent: array of int64;
  mincut_i: int64;
  mincut_temp: array of IntArray;
  mincut_row: array of int64;
  mincut_j: int64;
  mincut_path_flow: int64;
  mincut_s: int64;
  mincut_p: int64;
  mincut_cap: int64;
  mincut_v: int64;
  mincut_u: int64;
  mincut_res: array of IntArray;
begin
  mincut_g := mincut_graph;
  mincut_parent := [];
  mincut_i := 0;
  while mincut_i < Length(mincut_g) do begin
  mincut_parent := concat(mincut_parent, IntArray([-1]));
  mincut_i := mincut_i + 1;
end;
  mincut_temp := [];
  mincut_i := 0;
  while mincut_i < Length(mincut_g) do begin
  mincut_row := [];
  mincut_j := 0;
  while mincut_j < Length(mincut_g[mincut_i]) do begin
  mincut_row := concat(mincut_row, IntArray([mincut_g[mincut_i][mincut_j]]));
  mincut_j := mincut_j + 1;
end;
  mincut_temp := concat(mincut_temp, [mincut_row]);
  mincut_i := mincut_i + 1;
end;
  while bfs(mincut_g, mincut_source, mincut_sink, mincut_parent) do begin
  mincut_path_flow := 1000000000;
  mincut_s := mincut_sink;
  while mincut_s <> mincut_source do begin
  mincut_p := mincut_parent[mincut_s];
  mincut_cap := mincut_g[mincut_p][mincut_s];
  if mincut_cap < mincut_path_flow then begin
  mincut_path_flow := mincut_cap;
end;
  mincut_s := mincut_p;
end;
  mincut_v := mincut_sink;
  while mincut_v <> mincut_source do begin
  mincut_u := mincut_parent[mincut_v];
  mincut_g[mincut_u][mincut_v] := mincut_g[mincut_u][mincut_v] - mincut_path_flow;
  mincut_g[mincut_v][mincut_u] := mincut_g[mincut_v][mincut_u] + mincut_path_flow;
  mincut_v := mincut_u;
end;
end;
  mincut_res := [];
  mincut_i := 0;
  while mincut_i < Length(mincut_g) do begin
  mincut_j := 0;
  while mincut_j < Length(mincut_g[0]) do begin
  if (mincut_g[mincut_i][mincut_j] = 0) and (mincut_temp[mincut_i][mincut_j] > 0) then begin
  mincut_res := concat(mincut_res, [[mincut_i, mincut_j]]);
end;
  mincut_j := mincut_j + 1;
end;
  mincut_i := mincut_i + 1;
end;
  exit(mincut_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  test_graph := [[0, 16, 13, 0, 0, 0], [0, 0, 10, 12, 0, 0], [0, 4, 0, 0, 14, 0], [0, 0, 9, 0, 0, 20], [0, 0, 0, 7, 0, 4], [0, 0, 0, 0, 0, 0]];
  result_ := mincut(test_graph, 0, 5);
  writeln(list_list_int_to_str(result_));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
