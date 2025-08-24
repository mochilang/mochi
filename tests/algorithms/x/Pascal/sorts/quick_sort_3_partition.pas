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
function list_int_to_str(xs: array of int64): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  array1: array of int64;
  array2: array of int64;
  array3: array of int64;
  nums1: array of int64;
  nums2: array of int64;
  nums3: array of int64;
function quick_sort_3partition(quick_sort_3partition_arr: IntArray; quick_sort_3partition_left: int64; quick_sort_3partition_right: int64): IntArray; forward;
function quick_sort_lomuto_partition(quick_sort_lomuto_partition_arr: IntArray; quick_sort_lomuto_partition_left: int64; quick_sort_lomuto_partition_right: int64): IntArray; forward;
function lomuto_partition(lomuto_partition_arr: IntArray; lomuto_partition_left: int64; lomuto_partition_right: int64): int64; forward;
function three_way_radix_quicksort(three_way_radix_quicksort_arr: IntArray): IntArray; forward;
function quick_sort_3partition(quick_sort_3partition_arr: IntArray; quick_sort_3partition_left: int64; quick_sort_3partition_right: int64): IntArray;
var
  quick_sort_3partition_a: int64;
  quick_sort_3partition_i: int64;
  quick_sort_3partition_b: int64;
  quick_sort_3partition_pivot: int64;
  quick_sort_3partition_temp: int64;
  quick_sort_3partition_temp_9: int64;
begin
  if quick_sort_3partition_right <= quick_sort_3partition_left then begin
  exit(quick_sort_3partition_arr);
end;
  quick_sort_3partition_a := quick_sort_3partition_left;
  quick_sort_3partition_i := quick_sort_3partition_left;
  quick_sort_3partition_b := quick_sort_3partition_right;
  quick_sort_3partition_pivot := quick_sort_3partition_arr[quick_sort_3partition_left];
  while quick_sort_3partition_i <= quick_sort_3partition_b do begin
  if quick_sort_3partition_arr[quick_sort_3partition_i] < quick_sort_3partition_pivot then begin
  quick_sort_3partition_temp := quick_sort_3partition_arr[quick_sort_3partition_a];
  quick_sort_3partition_arr[quick_sort_3partition_a] := quick_sort_3partition_arr[quick_sort_3partition_i];
  quick_sort_3partition_arr[quick_sort_3partition_i] := quick_sort_3partition_temp;
  quick_sort_3partition_a := quick_sort_3partition_a + 1;
  quick_sort_3partition_i := quick_sort_3partition_i + 1;
end else begin
  if quick_sort_3partition_arr[quick_sort_3partition_i] > quick_sort_3partition_pivot then begin
  quick_sort_3partition_temp_9 := quick_sort_3partition_arr[quick_sort_3partition_b];
  quick_sort_3partition_arr[quick_sort_3partition_b] := quick_sort_3partition_arr[quick_sort_3partition_i];
  quick_sort_3partition_arr[quick_sort_3partition_i] := quick_sort_3partition_temp_9;
  quick_sort_3partition_b := quick_sort_3partition_b - 1;
end else begin
  quick_sort_3partition_i := quick_sort_3partition_i + 1;
end;
end;
end;
  quick_sort_3partition_arr := quick_sort_3partition(quick_sort_3partition_arr, quick_sort_3partition_left, quick_sort_3partition_a - 1);
  quick_sort_3partition_arr := quick_sort_3partition(quick_sort_3partition_arr, quick_sort_3partition_b + 1, quick_sort_3partition_right);
  exit(quick_sort_3partition_arr);
end;
function quick_sort_lomuto_partition(quick_sort_lomuto_partition_arr: IntArray; quick_sort_lomuto_partition_left: int64; quick_sort_lomuto_partition_right: int64): IntArray;
var
  quick_sort_lomuto_partition_pivot_index: int64;
begin
  if quick_sort_lomuto_partition_left < quick_sort_lomuto_partition_right then begin
  quick_sort_lomuto_partition_pivot_index := lomuto_partition(quick_sort_lomuto_partition_arr, quick_sort_lomuto_partition_left, quick_sort_lomuto_partition_right);
  quick_sort_lomuto_partition_arr := quick_sort_lomuto_partition(quick_sort_lomuto_partition_arr, quick_sort_lomuto_partition_left, quick_sort_lomuto_partition_pivot_index - 1);
  quick_sort_lomuto_partition_arr := quick_sort_lomuto_partition(quick_sort_lomuto_partition_arr, quick_sort_lomuto_partition_pivot_index + 1, quick_sort_lomuto_partition_right);
end;
  exit(quick_sort_lomuto_partition_arr);
end;
function lomuto_partition(lomuto_partition_arr: IntArray; lomuto_partition_left: int64; lomuto_partition_right: int64): int64;
var
  lomuto_partition_pivot: int64;
  lomuto_partition_store_index: int64;
  lomuto_partition_i: int64;
  lomuto_partition_temp: int64;
  lomuto_partition_temp_15: int64;
begin
  lomuto_partition_pivot := lomuto_partition_arr[lomuto_partition_right];
  lomuto_partition_store_index := lomuto_partition_left;
  lomuto_partition_i := lomuto_partition_left;
  while lomuto_partition_i < lomuto_partition_right do begin
  if lomuto_partition_arr[lomuto_partition_i] < lomuto_partition_pivot then begin
  lomuto_partition_temp := lomuto_partition_arr[lomuto_partition_store_index];
  lomuto_partition_arr[lomuto_partition_store_index] := lomuto_partition_arr[lomuto_partition_i];
  lomuto_partition_arr[lomuto_partition_i] := lomuto_partition_temp;
  lomuto_partition_store_index := lomuto_partition_store_index + 1;
end;
  lomuto_partition_i := lomuto_partition_i + 1;
end;
  lomuto_partition_temp_15 := lomuto_partition_arr[lomuto_partition_right];
  lomuto_partition_arr[lomuto_partition_right] := lomuto_partition_arr[lomuto_partition_store_index];
  lomuto_partition_arr[lomuto_partition_store_index] := lomuto_partition_temp_15;
  exit(lomuto_partition_store_index);
end;
function three_way_radix_quicksort(three_way_radix_quicksort_arr: IntArray): IntArray;
var
  three_way_radix_quicksort_pivot: int64;
  three_way_radix_quicksort_less: array of int64;
  three_way_radix_quicksort_equal: array of int64;
  three_way_radix_quicksort_greater: array of int64;
  three_way_radix_quicksort_i: int64;
  three_way_radix_quicksort_val: int64;
  three_way_radix_quicksort_sorted_less: array of int64;
  three_way_radix_quicksort_sorted_greater: array of int64;
  three_way_radix_quicksort_result_: array of int64;
begin
  if Length(three_way_radix_quicksort_arr) <= 1 then begin
  exit(three_way_radix_quicksort_arr);
end;
  three_way_radix_quicksort_pivot := three_way_radix_quicksort_arr[0];
  three_way_radix_quicksort_less := [];
  three_way_radix_quicksort_equal := [];
  three_way_radix_quicksort_greater := [];
  three_way_radix_quicksort_i := 0;
  while three_way_radix_quicksort_i < Length(three_way_radix_quicksort_arr) do begin
  three_way_radix_quicksort_val := three_way_radix_quicksort_arr[three_way_radix_quicksort_i];
  if three_way_radix_quicksort_val < three_way_radix_quicksort_pivot then begin
  three_way_radix_quicksort_less := concat(three_way_radix_quicksort_less, IntArray([three_way_radix_quicksort_val]));
end else begin
  if three_way_radix_quicksort_val > three_way_radix_quicksort_pivot then begin
  three_way_radix_quicksort_greater := concat(three_way_radix_quicksort_greater, IntArray([three_way_radix_quicksort_val]));
end else begin
  three_way_radix_quicksort_equal := concat(three_way_radix_quicksort_equal, IntArray([three_way_radix_quicksort_val]));
end;
end;
  three_way_radix_quicksort_i := three_way_radix_quicksort_i + 1;
end;
  three_way_radix_quicksort_sorted_less := three_way_radix_quicksort(three_way_radix_quicksort_less);
  three_way_radix_quicksort_sorted_greater := three_way_radix_quicksort(three_way_radix_quicksort_greater);
  three_way_radix_quicksort_result_ := concat(three_way_radix_quicksort_sorted_less, three_way_radix_quicksort_equal);
  three_way_radix_quicksort_result_ := concat(three_way_radix_quicksort_result_, three_way_radix_quicksort_sorted_greater);
  exit(three_way_radix_quicksort_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  array1 := [5, -1, -1, 5, 5, 24, 0];
  array1 := quick_sort_3partition(array1, 0, Length(array1) - 1);
  writeln(list_int_to_str(array1));
  array2 := [9, 0, 2, 6];
  array2 := quick_sort_3partition(array2, 0, Length(array2) - 1);
  writeln(list_int_to_str(array2));
  array3 := [];
  array3 := quick_sort_3partition(array3, 0, Length(array3) - 1);
  writeln(list_int_to_str(array3));
  nums1 := [0, 5, 3, 1, 2];
  nums1 := quick_sort_lomuto_partition(nums1, 0, Length(nums1) - 1);
  writeln(list_int_to_str(nums1));
  nums2 := [];
  nums2 := quick_sort_lomuto_partition(nums2, 0, Length(nums2) - 1);
  writeln(list_int_to_str(nums2));
  nums3 := [-2, 5, 0, -4];
  nums3 := quick_sort_lomuto_partition(nums3, 0, Length(nums3) - 1);
  writeln(list_int_to_str(nums3));
  writeln(list_int_to_str(three_way_radix_quicksort([])));
  writeln(list_int_to_str(three_way_radix_quicksort([1])));
  writeln(list_int_to_str(three_way_radix_quicksort([-5, -2, 1, -2, 0, 1])));
  writeln(list_int_to_str(three_way_radix_quicksort([1, 2, 5, 1, 2, 0, 0, 5, 2, -1])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
