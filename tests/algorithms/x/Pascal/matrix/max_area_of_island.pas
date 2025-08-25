{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
type IntArray = array of int64;
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
  matrix: array of IntArray;
function encode(encode_row: int64; encode_col: int64): string; forward;
function is_safe(is_safe_row: int64; is_safe_col: int64; is_safe_rows: int64; is_safe_cols: int64): boolean; forward;
function has(has_seen: specialize TFPGMap<string, boolean>; has_key: string): boolean; forward;
function depth_first_search(depth_first_search_row: int64; depth_first_search_col: int64; depth_first_search_seen: specialize TFPGMap<string, boolean>; depth_first_search_mat: IntArrayArray): int64; forward;
function find_max_area(find_max_area_mat: IntArrayArray): int64; forward;
function encode(encode_row: int64; encode_col: int64): string;
begin
  exit((IntToStr(encode_row) + ',') + IntToStr(encode_col));
end;
function is_safe(is_safe_row: int64; is_safe_col: int64; is_safe_rows: int64; is_safe_cols: int64): boolean;
begin
  exit((((is_safe_row >= 0) and (is_safe_row < is_safe_rows)) and (is_safe_col >= 0)) and (is_safe_col < is_safe_cols));
end;
function has(has_seen: specialize TFPGMap<string, boolean>; has_key: string): boolean;
begin
  exit(has_seen.IndexOf(has_key) <> -1);
end;
function depth_first_search(depth_first_search_row: int64; depth_first_search_col: int64; depth_first_search_seen: specialize TFPGMap<string, boolean>; depth_first_search_mat: IntArrayArray): int64;
var
  depth_first_search_rows: integer;
  depth_first_search_cols: integer;
  depth_first_search_key: string;
begin
  depth_first_search_rows := Length(depth_first_search_mat);
  depth_first_search_cols := Length(depth_first_search_mat[0]);
  depth_first_search_key := encode(depth_first_search_row, depth_first_search_col);
  if (is_safe(depth_first_search_row, depth_first_search_col, depth_first_search_rows, depth_first_search_cols) and not has(depth_first_search_seen, depth_first_search_key)) and (depth_first_search_mat[depth_first_search_row][depth_first_search_col] = 1) then begin
  depth_first_search_seen[depth_first_search_key] := true;
  exit((((1 + depth_first_search(depth_first_search_row + 1, depth_first_search_col, depth_first_search_seen, depth_first_search_mat)) + depth_first_search(depth_first_search_row - 1, depth_first_search_col, depth_first_search_seen, depth_first_search_mat)) + depth_first_search(depth_first_search_row, depth_first_search_col + 1, depth_first_search_seen, depth_first_search_mat)) + depth_first_search(depth_first_search_row, depth_first_search_col - 1, depth_first_search_seen, depth_first_search_mat));
end else begin
  exit(0);
end;
end;
function find_max_area(find_max_area_mat: IntArrayArray): int64;
var
  find_max_area_seen: specialize TFPGMap<string, boolean>;
  find_max_area_rows: integer;
  find_max_area_max_area: int64;
  find_max_area_r: int64;
  find_max_area_line: array of int64;
  find_max_area_cols: integer;
  find_max_area_c: int64;
  find_max_area_key: string;
  find_max_area_area: int64;
begin
  find_max_area_seen := specialize TFPGMap<string, boolean>.Create();
  find_max_area_rows := Length(find_max_area_mat);
  find_max_area_max_area := 0;
  find_max_area_r := 0;
  while find_max_area_r < find_max_area_rows do begin
  find_max_area_line := find_max_area_mat[find_max_area_r];
  find_max_area_cols := Length(find_max_area_line);
  find_max_area_c := 0;
  while find_max_area_c < find_max_area_cols do begin
  if find_max_area_line[find_max_area_c] = 1 then begin
  find_max_area_key := encode(find_max_area_r, find_max_area_c);
  if not find_max_area_seen.IndexOf(find_max_area_key) <> -1 then begin
  find_max_area_area := depth_first_search(find_max_area_r, find_max_area_c, find_max_area_seen, find_max_area_mat);
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
  writeln('');
end.
