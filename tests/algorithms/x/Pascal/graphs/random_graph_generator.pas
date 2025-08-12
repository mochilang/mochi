{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
type IntArray = array of integer;
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
procedure show_map_int_array(m: specialize TFPGMap<integer, IntArray>);
var i,j: integer;
begin
  write('map[');
  for i := 0 to m.Count - 1 do begin
    write(m.Keys[i]);
    write(':');
    write('[');
    for j := 0 to Length(m.Data[i]) - 1 do begin
      write(m.Data[i][j]);
      if j < Length(m.Data[i]) - 1 then write(' ');
    end;
    write(']');
    if i < m.Count - 1 then write(' ');
  end;
  writeln(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  seed: integer;
  directed: boolean;
  vertices_number: integer;
  probability: real;
function rand(): integer; forward;
function random(): real; forward;
function complete_graph(vertices_number: integer): specialize TFPGMap<integer, IntArray>; forward;
function random_graph(vertices_number: integer; probability: real; directed: boolean): specialize TFPGMap<integer, IntArray>; forward;
procedure main(); forward;
function rand(): integer;
begin
  seed := ((seed * 1103515245) + 12345) mod 2147483648;
  exit(seed);
end;
function random(): real;
begin
  exit((1 * rand()) / 2.147483648e+09);
end;
function complete_graph(vertices_number: integer): specialize TFPGMap<integer, IntArray>;
var
  complete_graph_graph: specialize TFPGMap<integer, IntArray>;
  complete_graph_i: integer;
  complete_graph_neighbors: array of integer;
  complete_graph_j: integer;
begin
  complete_graph_graph := specialize TFPGMap<integer, IntArray>.Create();
  complete_graph_i := 0;
  while complete_graph_i < vertices_number do begin
  complete_graph_neighbors := [];
  complete_graph_j := 0;
  while complete_graph_j < vertices_number do begin
  if complete_graph_j <> complete_graph_i then begin
  complete_graph_neighbors := concat(complete_graph_neighbors, IntArray([complete_graph_j]));
end;
  complete_graph_j := complete_graph_j + 1;
end;
  complete_graph_graph[complete_graph_i] := complete_graph_neighbors;
  complete_graph_i := complete_graph_i + 1;
end;
  exit(complete_graph_graph);
end;
function random_graph(vertices_number: integer; probability: real; directed: boolean): specialize TFPGMap<integer, IntArray>;
var
  random_graph_graph: specialize TFPGMap<integer, IntArray>;
  random_graph_i: integer;
  random_graph_j: integer;
begin
  random_graph_graph := specialize TFPGMap<integer, IntArray>.Create();
  random_graph_i := 0;
  while random_graph_i < vertices_number do begin
  random_graph_graph[random_graph_i] := [];
  random_graph_i := random_graph_i + 1;
end;
  if probability >= 1 then begin
  exit(complete_graph(vertices_number));
end;
  if probability <= 0 then begin
  exit(random_graph_graph);
end;
  random_graph_i := 0;
  while random_graph_i < vertices_number do begin
  random_graph_j := random_graph_i + 1;
  while random_graph_j < vertices_number do begin
  if random() < probability then begin
  random_graph_graph[random_graph_i] := concat(random_graph_graph[random_graph_i], IntArray([random_graph_j]));
  if not directed then begin
  random_graph_graph[random_graph_j] := concat(random_graph_graph[random_graph_j], IntArray([random_graph_i]));
end;
end;
  random_graph_j := random_graph_j + 1;
end;
  random_graph_i := random_graph_i + 1;
end;
  exit(random_graph_graph);
end;
procedure main();
var
  main_g1: specialize TFPGMap<integer, IntArray>;
  main_g2: specialize TFPGMap<integer, IntArray>;
begin
  seed := 1;
  main_g1 := random_graph(4, 0.5, false);
  show_map_int_array(main_g1);
  seed := 1;
  main_g2 := random_graph(4, 0.5, true);
  show_map_int_array(main_g2);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  seed := 1;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
