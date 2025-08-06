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
procedure json_intarray(xs: IntArray);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(',');
  end;
  write(']');
end;
procedure json(xs: IntArrayArray);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    if i > 0 then write(',');
    json_intarray(xs[i]);
  end;
  writeln(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function sum_list(nums: IntArray): integer; forward;
function create_state_space_tree(nums: IntArray; max_sum: integer; num_index: integer; path: IntArray; curr_sum: integer; remaining_sum: integer): IntArrayArray; forward;
function generate_sum_of_subsets_solutions(nums: IntArray; max_sum: integer): IntArrayArray; forward;
procedure main(); forward;
function sum_list(nums: IntArray): integer;
var
  sum_list_s: integer;
  sum_list_n: integer;
begin
  sum_list_s := 0;
  for sum_list_n in nums do begin
  sum_list_s := sum_list_s + sum_list_n;
end;
  exit(sum_list_s);
end;
function create_state_space_tree(nums: IntArray; max_sum: integer; num_index: integer; path: IntArray; curr_sum: integer; remaining_sum: integer): IntArrayArray;
var
  create_state_space_tree_result_: array of IntArray;
  create_state_space_tree_index: integer;
  create_state_space_tree_value: integer;
  create_state_space_tree_subres: array of IntArray;
  create_state_space_tree_j: integer;
begin
  create_state_space_tree_result_ := [];
  if (curr_sum > max_sum) or ((curr_sum + remaining_sum) < max_sum) then begin
  exit(create_state_space_tree_result_);
end;
  if curr_sum = max_sum then begin
  create_state_space_tree_result_ := concat(create_state_space_tree_result_, [path]);
  exit(create_state_space_tree_result_);
end;
  create_state_space_tree_index := num_index;
  while create_state_space_tree_index < Length(nums) do begin
  create_state_space_tree_value := nums[create_state_space_tree_index];
  create_state_space_tree_subres := create_state_space_tree(nums, max_sum, create_state_space_tree_index + 1, concat(path, [create_state_space_tree_value]), curr_sum + create_state_space_tree_value, remaining_sum - create_state_space_tree_value);
  create_state_space_tree_j := 0;
  while create_state_space_tree_j < Length(create_state_space_tree_subres) do begin
  create_state_space_tree_result_ := concat(create_state_space_tree_result_, [create_state_space_tree_subres[create_state_space_tree_j]]);
  create_state_space_tree_j := create_state_space_tree_j + 1;
end;
  create_state_space_tree_index := create_state_space_tree_index + 1;
end;
  exit(create_state_space_tree_result_);
end;
function generate_sum_of_subsets_solutions(nums: IntArray; max_sum: integer): IntArrayArray;
var
  generate_sum_of_subsets_solutions_total: integer;
begin
  generate_sum_of_subsets_solutions_total := sum_list(nums);
  exit(create_state_space_tree(nums, max_sum, 0, [], 0, generate_sum_of_subsets_solutions_total));
end;
procedure main();
begin
  json(generate_sum_of_subsets_solutions([3, 34, 4, 12, 5, 2], 9));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
