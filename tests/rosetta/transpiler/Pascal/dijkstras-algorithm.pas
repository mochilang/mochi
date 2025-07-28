{$mode objfpc}
program Main;
uses SysUtils, fgl;
type StrArray = array of string;
var
  INF: integer;
  graph: specialize TFPGMap<string, specialize TFPGMap<string, integer>>;
function Map2(): specialize TFPGMap<string, Variant>; forward;
function Map1(): specialize TFPGMap<string, >; forward;
procedure addEdge(u: string; v: string; w: integer); forward;
function removeAt(var xs: StrArray; idx: integer): StrArray; forward;
function dijkstra(source: string): specialize TFPGMap<string, Variant>; forward;
function path(prev: specialize TFPGMap<string, string>; v: string): string; forward;
procedure main(); forward;
function Map2(): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('dist', dijkstra_dist);
  Result.AddOrSetData('prev', dijkstra_prev);
end;
function Map1(): specialize TFPGMap<string, >;
begin
  Result := specialize TFPGMap<string, >.Create();
end;
procedure addEdge(u: string; v: string; w: integer);
begin
  if not graph.IndexOf(u) <> -1 then begin
  graph.AddOrSetData(u, Map1());
end;
  graph[u][v] := w;
  if not graph.IndexOf(v) <> -1 then begin
  graph.AddOrSetData(v, Map1());
end;
end;
function removeAt(var xs: StrArray; idx: integer): StrArray;
var
  removeAt_out: array of string;
  removeAt_i: integer;
  x: integer;
begin
  removeAt_out := [];
  removeAt_i := 0;
  for x in xs do begin
  if removeAt_i <> idx then begin
  removeAt_out := concat(removeAt_out, [x]);
end;
  removeAt_i := removeAt_i + 1;
end;
  exit(removeAt_out);
end;
function dijkstra(source: string): specialize TFPGMap<string, Variant>;
var
  dijkstra_dist: specialize TFPGMap<string, integer>;
  dijkstra_prev: specialize TFPGMap<string, string>;
  v: integer;
  dijkstra_q: array of string;
  dijkstra_bestIdx: integer;
  dijkstra_u: string;
  dijkstra_i: integer;
  dijkstra_v: string;
  dijkstra_alt: integer;
begin
  for v in graph do begin
  dijkstra_dist.AddOrSetData(v, INF);
  dijkstra_prev.AddOrSetData(v, '');
end;
  dijkstra_dist.AddOrSetData(source, 0);
  dijkstra_q := [];
  for v in graph do begin
  dijkstra_q := concat(dijkstra_q, [v]);
end;
  while Length(dijkstra_q) > 0 do begin
  dijkstra_bestIdx := 0;
  dijkstra_u := dijkstra_q[0];
  dijkstra_i := 1;
  while dijkstra_i < Length(dijkstra_q) do begin
  dijkstra_v := dijkstra_q[dijkstra_i];
  if dijkstra_dist[dijkstra_v] < dijkstra_dist[dijkstra_u] then begin
  dijkstra_u := dijkstra_v;
  dijkstra_bestIdx := dijkstra_i;
end;
  dijkstra_i := dijkstra_i + 1;
end;
  dijkstra_q := removeAt(dijkstra_q, dijkstra_bestIdx);
  for v in graph[dijkstra_u] do begin
  dijkstra_alt := dijkstra_dist[dijkstra_u] + graph[dijkstra_u][dijkstra_v];
  if dijkstra_alt < dijkstra_dist[dijkstra_v] then begin
  dijkstra_dist.AddOrSetData(dijkstra_v, dijkstra_alt);
  dijkstra_prev.AddOrSetData(dijkstra_v, dijkstra_u);
end;
end;
end;
  exit(Map2());
end;
function path(prev: specialize TFPGMap<string, string>; v: string): string;
var
  path_s: string;
  path_cur: string;
begin
  path_s := v;
  path_cur := v;
  while prev[path_cur] <> '' do begin
  path_cur := prev[path_cur];
  path_s := path_cur + path_s;
end;
  exit(path_s);
end;
procedure main();
var
  main_res: specialize TFPGMap<string, Variant>;
  main_dist: integer;
  main_prev: integer;
begin
  addEdge('a', 'b', 7);
  addEdge('a', 'c', 9);
  addEdge('a', 'f', 14);
  addEdge('b', 'c', 10);
  addEdge('b', 'd', 15);
  addEdge('c', 'd', 11);
  addEdge('c', 'f', 2);
  addEdge('d', 'e', 6);
  addEdge('e', 'f', 9);
  main_res := dijkstra('a');
  main_dist := main_res['dist'];
  main_prev := main_res['prev'];
  writeln((('Distance to e: ' + IntToStr(main_dist['e'])) + ', Path: ') + path(main_prev, 'e'));
  writeln((('Distance to f: ' + IntToStr(main_dist['f'])) + ', Path: ') + path(main_prev, 'f'));
end;
begin
  INF := 1000000000;
  graph := specialize TFPGMap<string, specialize TFPGMap<string, integer>>.Create();
  main();
end.
