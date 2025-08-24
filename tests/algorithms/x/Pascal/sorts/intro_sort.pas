{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
procedure show_list_int64(xs: array of int64);
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
  example1: array of int64;
  example2: array of int64;
function insertion_sort(insertion_sort_a: IntArray; insertion_sort_start: int64; insertion_sort_end_: int64): IntArray; forward;
function heapify(heapify_a: IntArray; heapify_index: int64; heapify_heap_size: int64): IntArray; forward;
function heap_sort(heap_sort_a: IntArray): IntArray; forward;
function median_of_3(median_of_3_arr: IntArray; median_of_3_first: int64; median_of_3_middle: int64; median_of_3_last: int64): int64; forward;
function partition(partition_arr: IntArray; partition_low: int64; partition_high: int64; partition_pivot: int64): int64; forward;
function int_log2(int_log2_n: int64): int64; forward;
function intro_sort(intro_sort_arr: IntArray; intro_sort_start: int64; intro_sort_end_: int64; intro_sort_size_threshold: int64; intro_sort_max_depth: int64): IntArray; forward;
procedure intro_sort_main(intro_sort_main_arr: IntArray); forward;
function insertion_sort(insertion_sort_a: IntArray; insertion_sort_start: int64; insertion_sort_end_: int64): IntArray;
var
  insertion_sort_arr: array of int64;
  insertion_sort_i: int64;
  insertion_sort_key: int64;
  insertion_sort_j: int64;
begin
  insertion_sort_arr := insertion_sort_a;
  insertion_sort_i := insertion_sort_start;
  while insertion_sort_i < insertion_sort_end_ do begin
  insertion_sort_key := insertion_sort_arr[insertion_sort_i];
  insertion_sort_j := insertion_sort_i;
  while (insertion_sort_j > insertion_sort_start) and (insertion_sort_arr[insertion_sort_j - 1] > insertion_sort_key) do begin
  insertion_sort_arr[insertion_sort_j] := insertion_sort_arr[insertion_sort_j - 1];
  insertion_sort_j := insertion_sort_j - 1;
end;
  insertion_sort_arr[insertion_sort_j] := insertion_sort_key;
  insertion_sort_i := insertion_sort_i + 1;
end;
  exit(insertion_sort_arr);
end;
function heapify(heapify_a: IntArray; heapify_index: int64; heapify_heap_size: int64): IntArray;
var
  heapify_arr: array of int64;
  heapify_largest: int64;
  heapify_left: int64;
  heapify_right: int64;
  heapify_temp: int64;
begin
  heapify_arr := heapify_a;
  heapify_largest := heapify_index;
  heapify_left := (2 * heapify_index) + 1;
  heapify_right := (2 * heapify_index) + 2;
  if (heapify_left < heapify_heap_size) and (heapify_arr[heapify_left] > heapify_arr[heapify_largest]) then begin
  heapify_largest := heapify_left;
end;
  if (heapify_right < heapify_heap_size) and (heapify_arr[heapify_right] > heapify_arr[heapify_largest]) then begin
  heapify_largest := heapify_right;
end;
  if heapify_largest <> heapify_index then begin
  heapify_temp := heapify_arr[heapify_index];
  heapify_arr[heapify_index] := heapify_arr[heapify_largest];
  heapify_arr[heapify_largest] := heapify_temp;
  heapify_arr := heapify(heapify_arr, heapify_largest, heapify_heap_size);
end;
  exit(heapify_arr);
end;
function heap_sort(heap_sort_a: IntArray): IntArray;
var
  heap_sort_arr: array of int64;
  heap_sort_n: integer;
  heap_sort_i: integer;
  heap_sort_temp: int64;
begin
  heap_sort_arr := heap_sort_a;
  heap_sort_n := Length(heap_sort_arr);
  if heap_sort_n <= 1 then begin
  exit(heap_sort_arr);
end;
  heap_sort_i := heap_sort_n div 2;
  while true do begin
  heap_sort_arr := heapify(heap_sort_arr, heap_sort_i, heap_sort_n);
  if heap_sort_i = 0 then begin
  break;
end;
  heap_sort_i := heap_sort_i - 1;
end;
  heap_sort_i := heap_sort_n - 1;
  while heap_sort_i > 0 do begin
  heap_sort_temp := heap_sort_arr[0];
  heap_sort_arr[0] := heap_sort_arr[heap_sort_i];
  heap_sort_arr[heap_sort_i] := heap_sort_temp;
  heap_sort_arr := heapify(heap_sort_arr, 0, heap_sort_i);
  heap_sort_i := heap_sort_i - 1;
end;
  exit(heap_sort_arr);
end;
function median_of_3(median_of_3_arr: IntArray; median_of_3_first: int64; median_of_3_middle: int64; median_of_3_last: int64): int64;
var
  median_of_3_a: int64;
  median_of_3_b: int64;
  median_of_3_c: int64;
begin
  median_of_3_a := median_of_3_arr[median_of_3_first];
  median_of_3_b := median_of_3_arr[median_of_3_middle];
  median_of_3_c := median_of_3_arr[median_of_3_last];
  if ((median_of_3_a > median_of_3_b) and (median_of_3_a < median_of_3_c)) or ((median_of_3_a < median_of_3_b) and (median_of_3_a > median_of_3_c)) then begin
  exit(median_of_3_a);
end else begin
  if ((median_of_3_b > median_of_3_a) and (median_of_3_b < median_of_3_c)) or ((median_of_3_b < median_of_3_a) and (median_of_3_b > median_of_3_c)) then begin
  exit(median_of_3_b);
end else begin
  exit(median_of_3_c);
end;
end;
end;
function partition(partition_arr: IntArray; partition_low: int64; partition_high: int64; partition_pivot: int64): int64;
var
  partition_i: int64;
  partition_j: int64;
  partition_temp: int64;
begin
  partition_i := partition_low;
  partition_j := partition_high;
  while true do begin
  while partition_arr[partition_i] < partition_pivot do begin
  partition_i := partition_i + 1;
end;
  partition_j := partition_j - 1;
  while partition_pivot < partition_arr[partition_j] do begin
  partition_j := partition_j - 1;
end;
  if partition_i >= partition_j then begin
  exit(partition_i);
end;
  partition_temp := partition_arr[partition_i];
  partition_arr[partition_i] := partition_arr[partition_j];
  partition_arr[partition_j] := partition_temp;
  partition_i := partition_i + 1;
end;
end;
function int_log2(int_log2_n: int64): int64;
var
  int_log2_v: int64;
  int_log2_r: int64;
begin
  int_log2_v := int_log2_n;
  int_log2_r := 0;
  while int_log2_v > 1 do begin
  int_log2_v := int_log2_v div 2;
  int_log2_r := int_log2_r + 1;
end;
  exit(int_log2_r);
end;
function intro_sort(intro_sort_arr: IntArray; intro_sort_start: int64; intro_sort_end_: int64; intro_sort_size_threshold: int64; intro_sort_max_depth: int64): IntArray;
var
  intro_sort_array_: array of int64;
  intro_sort_s: int64;
  intro_sort_e: int64;
  intro_sort_depth: int64;
  intro_sort_pivot: int64;
  intro_sort_p: int64;
  intro_sort_res: IntArray;
  intro_sort__: integer;
begin
  intro_sort_array_ := intro_sort_arr;
  intro_sort_s := intro_sort_start;
  intro_sort_e := intro_sort_end_;
  intro_sort_depth := intro_sort_max_depth;
  while (intro_sort_e - intro_sort_s) > intro_sort_size_threshold do begin
  if intro_sort_depth = 0 then begin
  exit(heap_sort(intro_sort_array_));
end;
  intro_sort_depth := intro_sort_depth - 1;
  intro_sort_pivot := median_of_3(intro_sort_array_, intro_sort_s, (intro_sort_s + ((intro_sort_e - intro_sort_s) div 2)) + 1, intro_sort_e - 1);
  intro_sort_p := partition(intro_sort_array_, intro_sort_s, intro_sort_e, intro_sort_pivot);
  intro_sort_array_ := intro_sort(intro_sort_array_, intro_sort_p, intro_sort_e, intro_sort_size_threshold, intro_sort_depth);
  intro_sort_e := intro_sort_p;
end;
  intro_sort_res := insertion_sort(intro_sort_array_, intro_sort_s, intro_sort_e);
  intro_sort__ := Length(intro_sort_res);
  exit(intro_sort_res);
end;
procedure intro_sort_main(intro_sort_main_arr: IntArray);
var
  intro_sort_main_max_depth: int64;
  intro_sort_main_sorted: IntArray;
begin
  if Length(intro_sort_main_arr) = 0 then begin
  show_list_int64(intro_sort_main_arr);
  exit();
end;
  intro_sort_main_max_depth := 2 * int_log2(Length(intro_sort_main_arr));
  intro_sort_main_sorted := intro_sort(intro_sort_main_arr, 0, Length(intro_sort_main_arr), 16, intro_sort_main_max_depth);
  show_list_int64(intro_sort_main_sorted);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  example1 := [4, 2, 6, 8, 1, 7, 8, 22, 14, 56, 27, 79, 23, 45, 14, 12];
  intro_sort_main(example1);
  example2 := [21, 15, 11, 45, -2, -11, 46];
  intro_sort_main(example2);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
