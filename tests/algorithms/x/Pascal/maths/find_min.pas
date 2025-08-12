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
  left: integer;
  nums: RealArray;
  right: integer;
function find_min_iterative(nums: RealArray): real; forward;
function find_min_recursive(nums: RealArray; left: integer; right: integer): real; forward;
procedure test_find_min(); forward;
procedure main(); forward;
function find_min_iterative(nums: RealArray): real;
var
  find_min_iterative_min_num: real;
  find_min_iterative_i: integer;
  find_min_iterative_num: real;
begin
  if Length(nums) = 0 then begin
  panic('find_min_iterative() arg is an empty sequence');
end;
  find_min_iterative_min_num := nums[0];
  find_min_iterative_i := 0;
  while find_min_iterative_i < Length(nums) do begin
  find_min_iterative_num := nums[find_min_iterative_i];
  if find_min_iterative_num < find_min_iterative_min_num then begin
  find_min_iterative_min_num := find_min_iterative_num;
end;
  find_min_iterative_i := find_min_iterative_i + 1;
end;
  exit(find_min_iterative_min_num);
end;
function find_min_recursive(nums: RealArray; left: integer; right: integer): real;
var
  find_min_recursive_n: integer;
  find_min_recursive_l: integer;
  find_min_recursive_r: integer;
  find_min_recursive_mid: integer;
  find_min_recursive_left_min: real;
  find_min_recursive_right_min: real;
begin
  find_min_recursive_n := Length(nums);
  if find_min_recursive_n = 0 then begin
  panic('find_min_recursive() arg is an empty sequence');
end;
  if (((left >= find_min_recursive_n) or (left < (0 - find_min_recursive_n))) or (right >= find_min_recursive_n)) or (right < (0 - find_min_recursive_n)) then begin
  panic('list index out of range');
end;
  find_min_recursive_l := left;
  find_min_recursive_r := right;
  if find_min_recursive_l < 0 then begin
  find_min_recursive_l := find_min_recursive_n + find_min_recursive_l;
end;
  if find_min_recursive_r < 0 then begin
  find_min_recursive_r := find_min_recursive_n + find_min_recursive_r;
end;
  if find_min_recursive_l = find_min_recursive_r then begin
  exit(nums[find_min_recursive_l]);
end;
  find_min_recursive_mid := (find_min_recursive_l + find_min_recursive_r) div 2;
  find_min_recursive_left_min := find_min_recursive(nums, find_min_recursive_l, find_min_recursive_mid);
  find_min_recursive_right_min := find_min_recursive(nums, find_min_recursive_mid + 1, find_min_recursive_r);
  if find_min_recursive_left_min <= find_min_recursive_right_min then begin
  exit(find_min_recursive_left_min);
end;
  exit(find_min_recursive_right_min);
end;
procedure test_find_min();
var
  test_find_min_a: array of real;
  test_find_min_b: array of real;
  test_find_min_c: array of real;
  test_find_min_d: array of real;
begin
  test_find_min_a := [3, 2, 1];
  if find_min_iterative(test_find_min_a) <> 1 then begin
  panic('iterative test1 failed');
end;
  if find_min_recursive(test_find_min_a, 0, Length(test_find_min_a) - 1) <> 1 then begin
  panic('recursive test1 failed');
end;
  test_find_min_b := [-3, -2, -1];
  if find_min_iterative(test_find_min_b) <> -3 then begin
  panic('iterative test2 failed');
end;
  if find_min_recursive(test_find_min_b, 0, Length(test_find_min_b) - 1) <> -3 then begin
  panic('recursive test2 failed');
end;
  test_find_min_c := [3, -3, 0];
  if find_min_iterative(test_find_min_c) <> -3 then begin
  panic('iterative test3 failed');
end;
  if find_min_recursive(test_find_min_c, 0, Length(test_find_min_c) - 1) <> -3 then begin
  panic('recursive test3 failed');
end;
  test_find_min_d := [1, 3, 5, 7, 9, 2, 4, 6, 8, 10];
  if find_min_recursive(test_find_min_d, 0 - Length(test_find_min_d), 0 - 1) <> 1 then begin
  panic('negative index test failed');
end;
end;
procedure main();
var
  main_sample: array of real;
begin
  test_find_min();
  main_sample := [0, 1, 2, 3, 4, 5, -3, 24, -56];
  writeln(FloatToStr(find_min_iterative(main_sample)));
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
