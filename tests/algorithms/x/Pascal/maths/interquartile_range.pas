{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
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
  nums: RealArray;
  a: real;
  b: real;
  x: real;
function bubble_sort(nums: RealArray): RealArray; forward;
function find_median(nums: RealArray): real; forward;
function interquartile_range(nums: RealArray): real; forward;
function absf(x: real): real; forward;
function float_equal(a: real; b: real): boolean; forward;
procedure test_interquartile_range(); forward;
procedure main(); forward;
function bubble_sort(nums: RealArray): RealArray;
var
  bubble_sort_arr: array of real;
  bubble_sort_i: integer;
  bubble_sort_n: integer;
  bubble_sort_a: integer;
  bubble_sort_b: integer;
  bubble_sort_temp: real;
begin
  bubble_sort_arr := [];
  bubble_sort_i := 0;
  while bubble_sort_i < Length(nums) do begin
  bubble_sort_arr := concat(bubble_sort_arr, [nums[bubble_sort_i]]);
  bubble_sort_i := bubble_sort_i + 1;
end;
  bubble_sort_n := Length(bubble_sort_arr);
  bubble_sort_a := 0;
  while bubble_sort_a < bubble_sort_n do begin
  bubble_sort_b := 0;
  while bubble_sort_b < ((bubble_sort_n - bubble_sort_a) - 1) do begin
  if bubble_sort_arr[bubble_sort_b] > bubble_sort_arr[bubble_sort_b + 1] then begin
  bubble_sort_temp := bubble_sort_arr[bubble_sort_b];
  bubble_sort_arr[bubble_sort_b] := bubble_sort_arr[bubble_sort_b + 1];
  bubble_sort_arr[bubble_sort_b + 1] := bubble_sort_temp;
end;
  bubble_sort_b := bubble_sort_b + 1;
end;
  bubble_sort_a := bubble_sort_a + 1;
end;
  exit(bubble_sort_arr);
end;
function find_median(nums: RealArray): real;
var
  find_median_length_: integer;
  find_median_div_: integer;
  find_median_mod_: integer;
begin
  find_median_length_ := Length(nums);
  find_median_div_ := find_median_length_ div 2;
  find_median_mod_ := find_median_length_ mod 2;
  if find_median_mod_ <> 0 then begin
  exit(nums[find_median_div_]);
end;
  exit((nums[find_median_div_] + nums[find_median_div_ - 1]) / 2);
end;
function interquartile_range(nums: RealArray): real;
var
  interquartile_range_sorted: RealArray;
  interquartile_range_length_: integer;
  interquartile_range_div_: integer;
  interquartile_range_mod_: integer;
  interquartile_range_lower: array of real;
  interquartile_range_i: integer;
  interquartile_range_upper: array of real;
  interquartile_range_j: integer;
  interquartile_range_q1: real;
  interquartile_range_q3: real;
begin
  if Length(nums) = 0 then begin
  panic('The list is empty. Provide a non-empty list.');
end;
  interquartile_range_sorted := bubble_sort(nums);
  interquartile_range_length_ := Length(interquartile_range_sorted);
  interquartile_range_div_ := interquartile_range_length_ div 2;
  interquartile_range_mod_ := interquartile_range_length_ mod 2;
  interquartile_range_lower := [];
  interquartile_range_i := 0;
  while interquartile_range_i < interquartile_range_div_ do begin
  interquartile_range_lower := concat(interquartile_range_lower, [interquartile_range_sorted[interquartile_range_i]]);
  interquartile_range_i := interquartile_range_i + 1;
end;
  interquartile_range_upper := [];
  interquartile_range_j := interquartile_range_div_ + interquartile_range_mod_;
  while interquartile_range_j < interquartile_range_length_ do begin
  interquartile_range_upper := concat(interquartile_range_upper, [interquartile_range_sorted[interquartile_range_j]]);
  interquartile_range_j := interquartile_range_j + 1;
end;
  interquartile_range_q1 := find_median(interquartile_range_lower);
  interquartile_range_q3 := find_median(interquartile_range_upper);
  exit(interquartile_range_q3 - interquartile_range_q1);
end;
function absf(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function float_equal(a: real; b: real): boolean;
var
  float_equal_diff: real;
begin
  float_equal_diff := absf(a - b);
  exit(float_equal_diff < 1e-07);
end;
procedure test_interquartile_range();
begin
  if not float_equal(interquartile_range([4, 1, 2, 3, 2]), 2) then begin
  panic('interquartile_range case1 failed');
end;
  if not float_equal(interquartile_range([-2, -7, -10, 9, 8, 4, -67, 45]), 17) then begin
  panic('interquartile_range case2 failed');
end;
  if not float_equal(interquartile_range([-2.1, -7.1, -10.1, 9.1, 8.1, 4.1, -67.1, 45.1]), 17.2) then begin
  panic('interquartile_range case3 failed');
end;
  if not float_equal(interquartile_range([0, 0, 0, 0, 0]), 0) then begin
  panic('interquartile_range case4 failed');
end;
end;
procedure main();
begin
  test_interquartile_range();
  writeln(FloatToStr(interquartile_range([4, 1, 2, 3, 2])));
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
end.
