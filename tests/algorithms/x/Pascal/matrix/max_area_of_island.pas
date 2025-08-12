{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
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
  matrix: array of IntArray;
  col: integer;
  mat: IntArrayArray;
  row: integer;
  key: string;
  cols: integer;
  rows: integer;
  seen: specialize TFPGMap<string, boolean>;
function encode(row: integer; col: integer): string; forward;
function is_safe(row: integer; col: integer; rows: integer; cols: integer): boolean; forward;
function has(seen: specialize TFPGMap<string, boolean>; key: string): boolean; forward;
function depth_first_search(row: integer; col: integer; seen: specialize TFPGMap<string, boolean>; mat: IntArrayArray): integer; forward;
function find_max_area(mat: IntArrayArray): integer; forward;
function encode(row: integer; col: integer): string;
begin
  exit((IntToStr(row) + ',') + IntToStr(col));
end;
function is_safe(row: integer; col: integer; rows: integer; cols: integer): boolean;
begin
  exit((((row >= 0) and (row < rows)) and (col >= 0)) and (col < cols));
end;
function has(seen: specialize TFPGMap<string, boolean>; key: string): boolean;
begin
  exit(seen.IndexOf(key) <> -1);
end;
function depth_first_search(row: integer; col: integer; seen: specialize TFPGMap<string, boolean>; mat: IntArrayArray): integer;
var
  depth_first_search_rows: integer;
  depth_first_search_cols: integer;
  depth_first_search_key: string;
begin
  depth_first_search_rows := Length(mat);
  depth_first_search_cols := Length(mat[0]);
  depth_first_search_key := encode(row, col);
  if (is_safe(row, col, depth_first_search_rows, depth_first_search_cols) and not has(seen, depth_first_search_key)) and (mat[row][col] = 1) then begin
  seen[depth_first_search_key] := true;
  exit((((1 + depth_first_search(row + 1, col, seen, mat)) + depth_first_search(row - 1, col, seen, mat)) + depth_first_search(row, col + 1, seen, mat)) + depth_first_search(row, col - 1, seen, mat));
end else begin
  exit(0);
end;
end;
function find_max_area(mat: IntArrayArray): integer;
var
  find_max_area_seen: specialize TFPGMap<string, boolean>;
  find_max_area_rows: integer;
  find_max_area_max_area: integer;
  find_max_area_r: integer;
  find_max_area_line: array of integer;
  find_max_area_cols: integer;
  find_max_area_c: integer;
  find_max_area_key: string;
  find_max_area_area: integer;
begin
  find_max_area_seen := specialize TFPGMap<string, boolean>.Create();
  find_max_area_rows := Length(mat);
  find_max_area_max_area := 0;
  find_max_area_r := 0;
  while find_max_area_r < find_max_area_rows do begin
  find_max_area_line := mat[find_max_area_r];
  find_max_area_cols := Length(find_max_area_line);
  find_max_area_c := 0;
  while find_max_area_c < find_max_area_cols do begin
  if find_max_area_line[find_max_area_c] = 1 then begin
  find_max_area_key := encode(find_max_area_r, find_max_area_c);
  if not find_max_area_seen.IndexOf(find_max_area_key) <> -1 then begin
  find_max_area_area := depth_first_search(find_max_area_r, find_max_area_c, find_max_area_seen, mat);
  if find_max_area_area > find_max_area_max_area then begin
  find_max_area_max_area := find_max_area_area;
end;
end;
end;
  find_max_area_c := find_max_area_c + 1;
end;
  find_max_area_r := find_max_area_r + 1;
end;
  exit(find_max_area_max_area);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  matrix := [[0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0], [0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0], [0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0], [0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0], [0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0]];
  writeln(find_max_area(matrix));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
