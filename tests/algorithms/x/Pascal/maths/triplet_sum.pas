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
function bubble_sort(bubble_sort_nums: IntArray): IntArray; forward;
function sort3(sort3_xs: IntArray): IntArray; forward;
function triplet_sum1(triplet_sum1_arr: IntArray; triplet_sum1_target: int64): IntArray; forward;
function triplet_sum2(triplet_sum2_arr: IntArray; triplet_sum2_target: int64): IntArray; forward;
function list_equal(list_equal_a: IntArray; list_equal_b: IntArray): boolean; forward;
procedure test_triplet_sum(); forward;
procedure main(); forward;
function bubble_sort(bubble_sort_nums: IntArray): IntArray;
var
  bubble_sort_arr: array of int64;
  bubble_sort_i: int64;
  bubble_sort_n: integer;
  bubble_sort_a: int64;
  bubble_sort_b: int64;
  bubble_sort_tmp: int64;
begin
  bubble_sort_arr := [];
  bubble_sort_i := 0;
  while bubble_sort_i < Length(bubble_sort_nums) do begin
  bubble_sort_arr := concat(bubble_sort_arr, IntArray([bubble_sort_nums[bubble_sort_i]]));
  bubble_sort_i := bubble_sort_i + 1;
end;
  bubble_sort_n := Length(bubble_sort_arr);
  bubble_sort_a := 0;
  while bubble_sort_a < bubble_sort_n do begin
  bubble_sort_b := 0;
  while bubble_sort_b < ((bubble_sort_n - bubble_sort_a) - 1) do begin
  if bubble_sort_arr[bubble_sort_b] > bubble_sort_arr[bubble_sort_b + 1] then begin
  bubble_sort_tmp := bubble_sort_arr[bubble_sort_b];
  bubble_sort_arr[bubble_sort_b] := bubble_sort_arr[bubble_sort_b + 1];
  bubble_sort_arr[bubble_sort_b + 1] := bubble_sort_tmp;
end;
  bubble_sort_b := bubble_sort_b + 1;
end;
  bubble_sort_a := bubble_sort_a + 1;
end;
  exit(bubble_sort_arr);
end;
function sort3(sort3_xs: IntArray): IntArray;
var
  sort3_arr: array of int64;
  sort3_i: int64;
  sort3_n: integer;
  sort3_a: int64;
  sort3_b: int64;
  sort3_tmp: int64;
begin
  sort3_arr := [];
  sort3_i := 0;
  while sort3_i < Length(sort3_xs) do begin
  sort3_arr := concat(sort3_arr, IntArray([sort3_xs[sort3_i]]));
  sort3_i := sort3_i + 1;
end;
  sort3_n := Length(sort3_arr);
  sort3_a := 0;
  while sort3_a < sort3_n do begin
  sort3_b := 0;
  while sort3_b < ((sort3_n - sort3_a) - 1) do begin
  if sort3_arr[sort3_b] > sort3_arr[sort3_b + 1] then begin
  sort3_tmp := sort3_arr[sort3_b];
  sort3_arr[sort3_b] := sort3_arr[sort3_b + 1];
  sort3_arr[sort3_b + 1] := sort3_tmp;
end;
  sort3_b := sort3_b + 1;
end;
  sort3_a := sort3_a + 1;
end;
  exit(sort3_arr);
end;
function triplet_sum1(triplet_sum1_arr: IntArray; triplet_sum1_target: int64): IntArray;
var
  triplet_sum1_i: int64;
  triplet_sum1_j: int64;
  triplet_sum1_k: int64;
begin
  triplet_sum1_i := 0;
  while triplet_sum1_i < (Length(triplet_sum1_arr) - 2) do begin
  triplet_sum1_j := triplet_sum1_i + 1;
  while triplet_sum1_j < (Length(triplet_sum1_arr) - 1) do begin
  triplet_sum1_k := triplet_sum1_j + 1;
  while triplet_sum1_k < Length(triplet_sum1_arr) do begin
  if ((triplet_sum1_arr[triplet_sum1_i] + triplet_sum1_arr[triplet_sum1_j]) + triplet_sum1_arr[triplet_sum1_k]) = triplet_sum1_target then begin
  exit(sort3([triplet_sum1_arr[triplet_sum1_i], triplet_sum1_arr[triplet_sum1_j], triplet_sum1_arr[triplet_sum1_k]]));
end;
  triplet_sum1_k := triplet_sum1_k + 1;
end;
  triplet_sum1_j := triplet_sum1_j + 1;
end;
  triplet_sum1_i := triplet_sum1_i + 1;
end;
  exit([0, 0, 0]);
end;
function triplet_sum2(triplet_sum2_arr: IntArray; triplet_sum2_target: int64): IntArray;
var
  triplet_sum2_sorted: IntArray;
  triplet_sum2_n: integer;
  triplet_sum2_i: int64;
  triplet_sum2_left: int64;
  triplet_sum2_right: integer;
  triplet_sum2_s: int64;
begin
  triplet_sum2_sorted := bubble_sort(triplet_sum2_arr);
  triplet_sum2_n := Length(triplet_sum2_sorted);
  triplet_sum2_i := 0;
  while triplet_sum2_i < (triplet_sum2_n - 2) do begin
  triplet_sum2_left := triplet_sum2_i + 1;
  triplet_sum2_right := triplet_sum2_n - 1;
  while triplet_sum2_left < triplet_sum2_right do begin
  triplet_sum2_s := (triplet_sum2_sorted[triplet_sum2_i] + triplet_sum2_sorted[triplet_sum2_left]) + triplet_sum2_sorted[triplet_sum2_right];
  if triplet_sum2_s = triplet_sum2_target then begin
  exit([triplet_sum2_sorted[triplet_sum2_i], triplet_sum2_sorted[triplet_sum2_left], triplet_sum2_sorted[triplet_sum2_right]]);
end;
  if triplet_sum2_s < triplet_sum2_target then begin
  triplet_sum2_left := triplet_sum2_left + 1;
end else begin
  triplet_sum2_right := triplet_sum2_right - 1;
end;
end;
  triplet_sum2_i := triplet_sum2_i + 1;
end;
  exit([0, 0, 0]);
end;
function list_equal(list_equal_a: IntArray; list_equal_b: IntArray): boolean;
var
  list_equal_i: int64;
begin
  if Length(list_equal_a) <> Length(list_equal_b) then begin
  exit(false);
end;
  list_equal_i := 0;
  while list_equal_i < Length(list_equal_a) do begin
  if list_equal_a[list_equal_i] <> list_equal_b[list_equal_i] then begin
  exit(false);
end;
  list_equal_i := list_equal_i + 1;
end;
  exit(true);
end;
procedure test_triplet_sum();
var
  test_triplet_sum_arr1: array of int64;
  test_triplet_sum_arr2: array of int64;
  test_triplet_sum_arr3: array of int64;
begin
  test_triplet_sum_arr1 := [13, 29, 7, 23, 5];
  if not list_equal(triplet_sum1(test_triplet_sum_arr1, 35), [5, 7, 23]) then begin
  panic('ts1 case1 failed');
end;
  if not list_equal(triplet_sum2(test_triplet_sum_arr1, 35), [5, 7, 23]) then begin
  panic('ts2 case1 failed');
end;
  test_triplet_sum_arr2 := [37, 9, 19, 50, 44];
  if not list_equal(triplet_sum1(test_triplet_sum_arr2, 65), [9, 19, 37]) then begin
  panic('ts1 case2 failed');
end;
  if not list_equal(triplet_sum2(test_triplet_sum_arr2, 65), [9, 19, 37]) then begin
  panic('ts2 case2 failed');
end;
  test_triplet_sum_arr3 := [6, 47, 27, 1, 15];
  if not list_equal(triplet_sum1(test_triplet_sum_arr3, 11), [0, 0, 0]) then begin
  panic('ts1 case3 failed');
end;
  if not list_equal(triplet_sum2(test_triplet_sum_arr3, 11), [0, 0, 0]) then begin
  panic('ts2 case3 failed');
end;
end;
procedure main();
var
  main_sample: array of int64;
  main_res: IntArray;
begin
  test_triplet_sum();
  main_sample := [13, 29, 7, 23, 5];
  main_res := triplet_sum2(main_sample, 35);
  writeln((((IntToStr(main_res[0]) + ' ') + IntToStr(main_res[1])) + ' ') + IntToStr(main_res[2]));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
