{$mode objfpc}
program Main;
uses SysUtils;
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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  colored_vertices: array of integer;
  graph: array of array of integer;
function valid_coloring(neighbours: IntArray; colored_vertices: IntArray; color_var: integer): boolean; forward;
function util_color(graph: IntArrayArray; max_colors: integer; colored_vertices: IntArray; index: integer): boolean; forward;
function color(graph: IntArrayArray; max_colors: integer): IntArray; forward;
function valid_coloring(neighbours: IntArray; colored_vertices: IntArray; color_var: integer): boolean;
var
  valid_coloring_i: integer;
begin
  valid_coloring_i := 0;
  while valid_coloring_i < Length(neighbours) do begin
  if (neighbours[valid_coloring_i] = 1) and (colored_vertices[valid_coloring_i] = color_var) then begin
  exit(false);
end;
  valid_coloring_i := valid_coloring_i + 1;
end;
  exit(true);
end;
function util_color(graph: IntArrayArray; max_colors: integer; colored_vertices: IntArray; index: integer): boolean;
var
  util_color_c: integer;
begin
  if index = Length(graph) then begin
  exit(true);
end;
  util_color_c := 0;
  while util_color_c < max_colors do begin
  if valid_coloring(graph[index], colored_vertices, util_color_c) then begin
  colored_vertices[index] := util_color_c;
  if util_color(graph, max_colors, colored_vertices, index + 1) then begin
  exit(true);
end;
  colored_vertices[index] := -1;
end;
  util_color_c := util_color_c + 1;
end;
  exit(false);
end;
function color(graph: IntArrayArray; max_colors: integer): IntArray;
var
  color_i: integer;
begin
  colored_vertices := [];
  color_i := 0;
  while color_i < Length(graph) do begin
  colored_vertices := concat(colored_vertices, [-1]);
  color_i := color_i + 1;
end;
  if util_color(graph, max_colors, colored_vertices, 0) then begin
  exit(colored_vertices);
end;
  exit([]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  graph := [[0, 1, 0, 0, 0], [1, 0, 1, 0, 1], [0, 1, 0, 1, 0], [0, 1, 1, 0, 0], [0, 1, 0, 0, 0]];
  show_list(color(graph, 3));
  writeln('' + #10 + '');
  writeln(Length(color(graph, 2)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
