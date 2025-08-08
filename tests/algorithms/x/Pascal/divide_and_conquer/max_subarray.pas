{$mode objfpc}
program Main;
uses SysUtils;
type Result = record
  start: integer;
  end_: integer;
  sum: real;
end;
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
  high: integer;
  mid: integer;
  arr: RealArray;
  low: integer;
  res: Result;
function makeResult(start: integer; end_: integer; sum: real): Result; forward;
function max_cross_sum(arr: RealArray; low: integer; mid: integer; high: integer): Result; forward;
function max_subarray(arr: RealArray; low: integer; high: integer): Result; forward;
procedure show(res: Result); forward;
procedure main(); forward;
function makeResult(start: integer; end_: integer; sum: real): Result;
begin
  Result.start := start;
  Result.end_ := end_;
  Result.sum := sum;
end;
function max_cross_sum(arr: RealArray; low: integer; mid: integer; high: integer): Result;
var
  max_cross_sum_left_sum: real;
  max_cross_sum_max_left: integer;
  max_cross_sum_sum: real;
  max_cross_sum_i: integer;
  max_cross_sum_right_sum: real;
  max_cross_sum_max_right: integer;
begin
  max_cross_sum_left_sum := -1e+18;
  max_cross_sum_max_left := -1;
  max_cross_sum_sum := 0;
  max_cross_sum_i := mid;
  while max_cross_sum_i >= low do begin
  max_cross_sum_sum := max_cross_sum_sum + arr[max_cross_sum_i];
  if max_cross_sum_sum > max_cross_sum_left_sum then begin
  max_cross_sum_left_sum := max_cross_sum_sum;
  max_cross_sum_max_left := max_cross_sum_i;
end;
  max_cross_sum_i := max_cross_sum_i - 1;
end;
  max_cross_sum_right_sum := -1e+18;
  max_cross_sum_max_right := -1;
  max_cross_sum_sum := 0;
  max_cross_sum_i := mid + 1;
  while max_cross_sum_i <= high do begin
  max_cross_sum_sum := max_cross_sum_sum + arr[max_cross_sum_i];
  if max_cross_sum_sum > max_cross_sum_right_sum then begin
  max_cross_sum_right_sum := max_cross_sum_sum;
  max_cross_sum_max_right := max_cross_sum_i;
end;
  max_cross_sum_i := max_cross_sum_i + 1;
end;
  exit(makeResult(max_cross_sum_max_left, max_cross_sum_max_right, max_cross_sum_left_sum + max_cross_sum_right_sum));
end;
function max_subarray(arr: RealArray; low: integer; high: integer): Result;
var
  max_subarray_mid: integer;
  max_subarray_left: Result;
  max_subarray_right: Result;
  max_subarray_cross: Result;
begin
  if Length(arr) = 0 then begin
  exit(makeResult(-1, -1, 0));
end;
  if low = high then begin
  exit(makeResult(low, high, arr[low]));
end;
  max_subarray_mid := (low + high) div 2;
  max_subarray_left := max_subarray(arr, low, max_subarray_mid);
  max_subarray_right := max_subarray(arr, max_subarray_mid + 1, high);
  max_subarray_cross := max_cross_sum(arr, low, max_subarray_mid, high);
  if (max_subarray_left.sum >= max_subarray_right.sum) and (max_subarray_left.sum >= max_subarray_cross.sum) then begin
  exit(max_subarray_left);
end;
  if (max_subarray_right.sum >= max_subarray_left.sum) and (max_subarray_right.sum >= max_subarray_cross.sum) then begin
  exit(max_subarray_right);
end;
  exit(max_subarray_cross);
end;
procedure show(res: Result);
begin
  writeln(((((('[' + IntToStr(res.start)) + ', ') + IntToStr(res.end_)) + ', ') + FloatToStr(res.sum)) + ']');
end;
procedure main();
var
  main_nums1: array of real;
  main_res1: Result;
  main_nums2: array of real;
  main_res2: Result;
  main_nums3: array of real;
  main_res3: Result;
  main_nums4: array of real;
  main_res4: Result;
  main_nums5: array of real;
  main_res5: Result;
  main_nums6: array of real;
  main_res6: Result;
begin
  main_nums1 := [-2, 1, -3, 4, -1, 2, 1, -5, 4];
  main_res1 := max_subarray(main_nums1, 0, Length(main_nums1) - 1);
  show(main_res1);
  main_nums2 := [2, 8, 9];
  main_res2 := max_subarray(main_nums2, 0, Length(main_nums2) - 1);
  show(main_res2);
  main_nums3 := [0, 0];
  main_res3 := max_subarray(main_nums3, 0, Length(main_nums3) - 1);
  show(main_res3);
  main_nums4 := [-1, 0, 1];
  main_res4 := max_subarray(main_nums4, 0, Length(main_nums4) - 1);
  show(main_res4);
  main_nums5 := [-2, -3, -1, -4, -6];
  main_res5 := max_subarray(main_nums5, 0, Length(main_nums5) - 1);
  show(main_res5);
  main_nums6 := [];
  main_res6 := max_subarray(main_nums6, 0, 0);
  show(main_res6);
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
