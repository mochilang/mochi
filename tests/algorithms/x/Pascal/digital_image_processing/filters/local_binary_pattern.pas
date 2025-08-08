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
  image: array of IntArray;
  i: integer;
  j: integer;
  line: string;
  value: integer;
  x: integer;
  y: integer;
  center: integer;
function get_neighbors_pixel(image: IntArrayArray; x: integer; y: integer; center: integer): integer; forward;
function local_binary_value(image: IntArrayArray; x: integer; y: integer): integer; forward;
function get_neighbors_pixel(image: IntArrayArray; x: integer; y: integer; center: integer): integer;
begin
  if (x < 0) or (y < 0) then begin
  exit(0);
end;
  if (x >= Length(image)) or (y >= Length(image[0])) then begin
  exit(0);
end;
  if image[x][y] >= center then begin
  exit(1);
end;
  exit(0);
end;
function local_binary_value(image: IntArrayArray; x: integer; y: integer): integer;
var
  local_binary_value_center: integer;
  local_binary_value_powers: array of integer;
  local_binary_value_neighbors: array of integer;
  local_binary_value_sum: integer;
  local_binary_value_i: integer;
begin
  local_binary_value_center := image[x][y];
  local_binary_value_powers := [1, 2, 4, 8, 16, 32, 64, 128];
  local_binary_value_neighbors := [get_neighbors_pixel(image, x - 1, y + 1, local_binary_value_center), get_neighbors_pixel(image, x, y + 1, local_binary_value_center), get_neighbors_pixel(image, x - 1, y, local_binary_value_center), get_neighbors_pixel(image, x + 1, y + 1, local_binary_value_center), get_neighbors_pixel(image, x + 1, y, local_binary_value_center), get_neighbors_pixel(image, x + 1, y - 1, local_binary_value_center), get_neighbors_pixel(image, x, y - 1, local_binary_value_center), get_neighbors_pixel(image, x - 1, y - 1, local_binary_value_center)];
  local_binary_value_sum := 0;
  local_binary_value_i := 0;
  while local_binary_value_i < Length(local_binary_value_neighbors) do begin
  local_binary_value_sum := local_binary_value_sum + (local_binary_value_neighbors[local_binary_value_i] * local_binary_value_powers[local_binary_value_i]);
  local_binary_value_i := local_binary_value_i + 1;
end;
  exit(local_binary_value_sum);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  image := [[10, 10, 10, 10, 10], [10, 20, 30, 20, 10], [10, 30, 40, 30, 10], [10, 20, 30, 20, 10], [10, 10, 10, 10, 10]];
  i := 0;
  while i < Length(image) do begin
  j := 0;
  line := '';
  while j < Length(image[0]) do begin
  value := local_binary_value(image, i, j);
  line := line + IntToStr(value);
  if j < (Length(image[0]) - 1) then begin
  line := line + ' ';
end;
  j := j + 1;
end;
  writeln(line);
  i := i + 1;
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
