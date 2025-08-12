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
  index: integer;
  right: integer;
  n: integer;
  nums: RealArray;
function normalize_index(index: integer; n: integer): integer; forward;
function find_max_iterative(nums: RealArray): real; forward;
function find_max_recursive(nums: RealArray; left: integer; right: integer): real; forward;
procedure test_find_max(); forward;
procedure main(); forward;
function normalize_index(index: integer; n: integer): integer;
begin
  if index < 0 then begin
  exit(n + index);
end;
  exit(index);
end;
function find_max_iterative(nums: RealArray): real;
var
  find_max_iterative_max_num: real;
  find_max_iterative_i: integer;
  find_max_iterative_x: real;
begin
  if Length(nums) = 0 then begin
  panic('find_max_iterative() arg is an empty sequence');
end;
  find_max_iterative_max_num := nums[0];
  find_max_iterative_i := 0;
  while find_max_iterative_i < Length(nums) do begin
  find_max_iterative_x := nums[find_max_iterative_i];
  if find_max_iterative_x > find_max_iterative_max_num then begin
  find_max_iterative_max_num := find_max_iterative_x;
end;
  find_max_iterative_i := find_max_iterative_i + 1;
end;
  exit(find_max_iterative_max_num);
end;
function find_max_recursive(nums: RealArray; left: integer; right: integer): real;
var
  find_max_recursive_n: integer;
  find_max_recursive_l: integer;
  find_max_recursive_r: integer;
  find_max_recursive_mid: integer;
  find_max_recursive_left_max: real;
  find_max_recursive_right_max: real;
begin
  find_max_recursive_n := Length(nums);
  if find_max_recursive_n = 0 then begin
  panic('find_max_recursive() arg is an empty sequence');
end;
  if (((left >= find_max_recursive_n) or (left < (0 - find_max_recursive_n))) or (right >= find_max_recursive_n)) or (right < (0 - find_max_recursive_n)) then begin
  panic('list index out of range');
end;
  find_max_recursive_l := normalize_index(left, find_max_recursive_n);
  find_max_recursive_r := normalize_index(right, find_max_recursive_n);
  if find_max_recursive_l = find_max_recursive_r then begin
  exit(nums[find_max_recursive_l]);
end;
  find_max_recursive_mid := (find_max_recursive_l + find_max_recursive_r) div 2;
  find_max_recursive_left_max := find_max_recursive(nums, find_max_recursive_l, find_max_recursive_mid);
  find_max_recursive_right_max := find_max_recursive(nums, find_max_recursive_mid + 1, find_max_recursive_r);
  if find_max_recursive_left_max >= find_max_recursive_right_max then begin
  exit(find_max_recursive_left_max);
end;
  exit(find_max_recursive_right_max);
end;
procedure test_find_max();
var
  test_find_max_arr: array of real;
begin
  test_find_max_arr := [2, 4, 9, 7, 19, 94, 5];
  if find_max_iterative(test_find_max_arr) <> 94 then begin
  panic('find_max_iterative failed');
end;
  if find_max_recursive(test_find_max_arr, 0, Length(test_find_max_arr) - 1) <> 94 then begin
  panic('find_max_recursive failed');
end;
  if find_max_recursive(test_find_max_arr, -Length(test_find_max_arr), -1) <> 94 then begin
  panic('negative index handling failed');
end;
end;
procedure main();
var
  main_nums: array of real;
begin
  test_find_max();
  main_nums := [2, 4, 9, 7, 19, 94, 5];
  writeln(find_max_iterative(main_nums));
  writeln(find_max_recursive(main_nums, 0, Length(main_nums) - 1));
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
