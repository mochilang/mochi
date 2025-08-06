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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function valid_connection(graph: IntArrayArray; next_ver: integer; curr_ind: integer; path: IntArray): boolean; forward;
function util_hamilton_cycle(graph: IntArrayArray; path: IntArray; curr_ind: integer): boolean; forward;
function hamilton_cycle(graph: IntArrayArray; start_index: integer): IntArray; forward;
function valid_connection(graph: IntArrayArray; next_ver: integer; curr_ind: integer; path: IntArray): boolean;
var
  valid_connection_v: integer;
begin
  if graph[path[curr_ind - 1]][next_ver] = 0 then begin
  exit(false);
end;
  for valid_connection_v in path do begin
  if valid_connection_v = next_ver then begin
  exit(false);
end;
end;
  exit(true);
end;
function util_hamilton_cycle(graph: IntArrayArray; path: IntArray; curr_ind: integer): boolean;
var
  util_hamilton_cycle_next_ver: integer;
begin
  if curr_ind = Length(graph) then begin
  exit(graph[path[curr_ind - 1]][path[0]] = 1);
end;
  util_hamilton_cycle_next_ver := 0;
  while util_hamilton_cycle_next_ver < Length(graph) do begin
  if valid_connection(graph, util_hamilton_cycle_next_ver, curr_ind, path) then begin
  path[curr_ind] := util_hamilton_cycle_next_ver;
  if util_hamilton_cycle(graph, path, curr_ind + 1) then begin
  exit(true);
end;
  path[curr_ind] := -1;
end;
  util_hamilton_cycle_next_ver := util_hamilton_cycle_next_ver + 1;
end;
  exit(false);
end;
function hamilton_cycle(graph: IntArrayArray; start_index: integer): IntArray;
var
  hamilton_cycle_path: array of integer;
  hamilton_cycle_i: integer;
  hamilton_cycle_last: integer;
begin
  hamilton_cycle_i := 0;
  while hamilton_cycle_i < (Length(graph) + 1) do begin
  hamilton_cycle_path[hamilton_cycle_i] := -1;
  hamilton_cycle_i := hamilton_cycle_i + 1;
end;
  hamilton_cycle_path[0] := start_index;
  hamilton_cycle_last := Length(hamilton_cycle_path) - 1;
  hamilton_cycle_path[hamilton_cycle_last] := start_index;
  if util_hamilton_cycle(graph, hamilton_cycle_path, 1) then begin
  exit(hamilton_cycle_path);
end;
  exit([]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
