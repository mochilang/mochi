{$mode objfpc}{$modeswitch nestedprocvars}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  matrix1: array of IntArray;
  matrix2: array of IntArray;
function bubble_sort(bubble_sort_a: IntArray): IntArray; forward;
function median(median_matrix: IntArrayArray): integer; forward;
function bubble_sort(bubble_sort_a: IntArray): IntArray;
var
  bubble_sort_arr: array of integer;
  bubble_sort_n: integer;
  bubble_sort_i: integer;
  bubble_sort_j: integer;
  bubble_sort_temp: integer;
begin
  bubble_sort_arr := bubble_sort_a;
  bubble_sort_n := Length(bubble_sort_arr);
  bubble_sort_i := 0;
  while bubble_sort_i < bubble_sort_n do begin
  bubble_sort_j := 0;
  while (bubble_sort_j + 1) < (bubble_sort_n - bubble_sort_i) do begin
  if bubble_sort_arr[bubble_sort_j] > bubble_sort_arr[bubble_sort_j + 1] then begin
  bubble_sort_temp := bubble_sort_arr[bubble_sort_j];
  bubble_sort_arr[bubble_sort_j] := bubble_sort_arr[bubble_sort_j + 1];
  bubble_sort_arr[bubble_sort_j + 1] := bubble_sort_temp;
end;
  bubble_sort_j := bubble_sort_j + 1;
end;
  bubble_sort_i := bubble_sort_i + 1;
end;
  exit(bubble_sort_arr);
end;
function median(median_matrix: IntArrayArray): integer;
var
  median_linear: array of integer;
  median_i: integer;
  median_row: array of integer;
  median_j: integer;
  median_sorted: IntArray;
  median_mid: integer;
begin
  median_linear := [];
  median_i := 0;
  while median_i < Length(median_matrix) do begin
  median_row := median_matrix[median_i];
  median_j := 0;
  while median_j < Length(median_row) do begin
  median_linear := concat(median_linear, IntArray([median_row[median_j]]));
  median_j := median_j + 1;
end;
  median_i := median_i + 1;
end;
  median_sorted := bubble_sort(median_linear);
  median_mid := (Length(median_sorted) - 1) div 2;
  exit(median_sorted[median_mid]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  matrix1 := [[1, 3, 5], [2, 6, 9], [3, 6, 9]];
  writeln(IntToStr(median(matrix1)));
  matrix2 := [[1, 2, 3], [4, 5, 6]];
  writeln(IntToStr(median(matrix2)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
