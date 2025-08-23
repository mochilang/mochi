{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  grid: array of array of int64;
function max_product_four(max_product_four_grid: IntArrayArray): int64; forward;
function max_product_four(max_product_four_grid: IntArrayArray): int64;
var
  max_product_four_maximum: int64;
  max_product_four_i: int64;
  max_product_four_j: int64;
  max_product_four_temp: int64;
begin
  max_product_four_maximum := 0;
  max_product_four_i := 0;
  while max_product_four_i < 20 do begin
  max_product_four_j := 0;
  while max_product_four_j < 17 do begin
  max_product_four_temp := ((max_product_four_grid[max_product_four_i][max_product_four_j] * max_product_four_grid[max_product_four_i][max_product_four_j + 1]) * max_product_four_grid[max_product_four_i][max_product_four_j + 2]) * max_product_four_grid[max_product_four_i][max_product_four_j + 3];
  if max_product_four_temp > max_product_four_maximum then begin
  max_product_four_maximum := max_product_four_temp;
end;
  max_product_four_j := max_product_four_j + 1;
end;
  max_product_four_i := max_product_four_i + 1;
end;
  max_product_four_i := 0;
  while max_product_four_i < 17 do begin
  max_product_four_j := 0;
  while max_product_four_j < 20 do begin
  max_product_four_temp := ((max_product_four_grid[max_product_four_i][max_product_four_j] * max_product_four_grid[max_product_four_i + 1][max_product_four_j]) * max_product_four_grid[max_product_four_i + 2][max_product_four_j]) * max_product_four_grid[max_product_four_i + 3][max_product_four_j];
  if max_product_four_temp > max_product_four_maximum then begin
  max_product_four_maximum := max_product_four_temp;
end;
  max_product_four_j := max_product_four_j + 1;
end;
  max_product_four_i := max_product_four_i + 1;
end;
  max_product_four_i := 0;
  while max_product_four_i < 17 do begin
  max_product_four_j := 0;
  while max_product_four_j < 17 do begin
  max_product_four_temp := ((max_product_four_grid[max_product_four_i][max_product_four_j] * max_product_four_grid[max_product_four_i + 1][max_product_four_j + 1]) * max_product_four_grid[max_product_four_i + 2][max_product_four_j + 2]) * max_product_four_grid[max_product_four_i + 3][max_product_four_j + 3];
  if max_product_four_temp > max_product_four_maximum then begin
  max_product_four_maximum := max_product_four_temp;
end;
  max_product_four_j := max_product_four_j + 1;
end;
  max_product_four_i := max_product_four_i + 1;
end;
  max_product_four_i := 0;
  while max_product_four_i < 17 do begin
  max_product_four_j := 3;
  while max_product_four_j < 20 do begin
  max_product_four_temp := ((max_product_four_grid[max_product_four_i][max_product_four_j] * max_product_four_grid[max_product_four_i + 1][max_product_four_j - 1]) * max_product_four_grid[max_product_four_i + 2][max_product_four_j - 2]) * max_product_four_grid[max_product_four_i + 3][max_product_four_j - 3];
  if max_product_four_temp > max_product_four_maximum then begin
  max_product_four_maximum := max_product_four_temp;
end;
  max_product_four_j := max_product_four_j + 1;
end;
  max_product_four_i := max_product_four_i + 1;
end;
  exit(max_product_four_maximum);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  grid := [[8, 2, 22, 97, 38, 15, 0, 40, 0, 75, 4, 5, 7, 78, 52, 12, 50, 77, 91, 8], [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 4, 56, 62, 0], [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 3, 49, 13, 36, 65], [52, 70, 95, 23, 4, 60, 11, 42, 69, 24, 68, 56, 1, 32, 56, 71, 37, 2, 36, 91], [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80], [24, 47, 32, 60, 99, 3, 45, 2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50], [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70], [67, 26, 20, 68, 2, 62, 12, 20, 95, 63, 94, 39, 63, 8, 40, 91, 66, 49, 94, 21], [24, 55, 58, 5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72], [21, 36, 23, 9, 75, 0, 76, 44, 20, 45, 35, 14, 0, 61, 33, 97, 34, 31, 33, 95], [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 3, 80, 4, 62, 16, 14, 9, 53, 56, 92], [16, 39, 5, 42, 96, 35, 31, 47, 55, 58, 88, 24, 0, 17, 54, 24, 36, 29, 85, 57], [86, 56, 0, 48, 35, 71, 89, 7, 5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58], [19, 80, 81, 68, 5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 4, 89, 55, 40], [4, 52, 8, 83, 97, 35, 99, 16, 7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66], [88, 36, 68, 87, 57, 62, 20, 72, 3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69], [4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 8, 46, 29, 32, 40, 62, 76, 36], [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 4, 36, 16], [20, 73, 35, 29, 78, 31, 90, 1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 5, 54], [1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 1, 89, 19, 67, 48]];
  writeln(IntToStr(max_product_four(grid)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
