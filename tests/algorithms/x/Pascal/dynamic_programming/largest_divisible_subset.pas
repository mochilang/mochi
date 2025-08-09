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
function list_int_to_str(xs: array of integer): string;
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
  items: IntArray;
  nums: IntArray;
function sort_list(nums: IntArray): IntArray; forward;
function largest_divisible_subset(items: IntArray): IntArray; forward;
procedure main(); forward;
function sort_list(nums: IntArray): IntArray;
var
  sort_list_arr: array of integer;
  sort_list_i: integer;
  sort_list_key: integer;
  sort_list_j: integer;
begin
  sort_list_arr := nums;
  sort_list_i := 1;
  while sort_list_i < Length(sort_list_arr) do begin
  sort_list_key := sort_list_arr[sort_list_i];
  sort_list_j := sort_list_i - 1;
  while (sort_list_j >= 0) and (sort_list_arr[sort_list_j] > sort_list_key) do begin
  sort_list_arr[sort_list_j + 1] := sort_list_arr[sort_list_j];
  sort_list_j := sort_list_j - 1;
end;
  sort_list_arr[sort_list_j + 1] := sort_list_key;
  sort_list_i := sort_list_i + 1;
end;
  exit(sort_list_arr);
end;
function largest_divisible_subset(items: IntArray): IntArray;
var
  largest_divisible_subset_nums: IntArray;
  largest_divisible_subset_n: integer;
  largest_divisible_subset_memo: array of integer;
  largest_divisible_subset_prev: array of integer;
  largest_divisible_subset_i: integer;
  largest_divisible_subset_j: integer;
  largest_divisible_subset_ans: integer;
  largest_divisible_subset_last_index: integer;
  largest_divisible_subset_result_: array of integer;
begin
  if Length(items) = 0 then begin
  exit([]);
end;
  largest_divisible_subset_nums := sort_list(items);
  largest_divisible_subset_n := Length(largest_divisible_subset_nums);
  largest_divisible_subset_memo := [];
  largest_divisible_subset_prev := [];
  largest_divisible_subset_i := 0;
  while largest_divisible_subset_i < largest_divisible_subset_n do begin
  largest_divisible_subset_memo := concat(largest_divisible_subset_memo, IntArray([1]));
  largest_divisible_subset_prev := concat(largest_divisible_subset_prev, IntArray([largest_divisible_subset_i]));
  largest_divisible_subset_i := largest_divisible_subset_i + 1;
end;
  largest_divisible_subset_i := 0;
  while largest_divisible_subset_i < largest_divisible_subset_n do begin
  largest_divisible_subset_j := 0;
  while largest_divisible_subset_j < largest_divisible_subset_i do begin
  if ((largest_divisible_subset_nums[largest_divisible_subset_j] = 0) or ((largest_divisible_subset_nums[largest_divisible_subset_i] mod largest_divisible_subset_nums[largest_divisible_subset_j]) = 0)) and ((largest_divisible_subset_memo[largest_divisible_subset_j] + 1) > largest_divisible_subset_memo[largest_divisible_subset_i]) then begin
  largest_divisible_subset_memo[largest_divisible_subset_i] := largest_divisible_subset_memo[largest_divisible_subset_j] + 1;
  largest_divisible_subset_prev[largest_divisible_subset_i] := largest_divisible_subset_j;
end;
  largest_divisible_subset_j := largest_divisible_subset_j + 1;
end;
  largest_divisible_subset_i := largest_divisible_subset_i + 1;
end;
  largest_divisible_subset_ans := 0 - 1;
  largest_divisible_subset_last_index := 0 - 1;
  largest_divisible_subset_i := 0;
  while largest_divisible_subset_i < largest_divisible_subset_n do begin
  if largest_divisible_subset_memo[largest_divisible_subset_i] > largest_divisible_subset_ans then begin
  largest_divisible_subset_ans := largest_divisible_subset_memo[largest_divisible_subset_i];
  largest_divisible_subset_last_index := largest_divisible_subset_i;
end;
  largest_divisible_subset_i := largest_divisible_subset_i + 1;
end;
  if largest_divisible_subset_last_index = (0 - 1) then begin
  exit([]);
end;
  largest_divisible_subset_result_ := [largest_divisible_subset_nums[largest_divisible_subset_last_index]];
  while largest_divisible_subset_prev[largest_divisible_subset_last_index] <> largest_divisible_subset_last_index do begin
  largest_divisible_subset_last_index := largest_divisible_subset_prev[largest_divisible_subset_last_index];
  largest_divisible_subset_result_ := concat(largest_divisible_subset_result_, IntArray([largest_divisible_subset_nums[largest_divisible_subset_last_index]]));
end;
  exit(largest_divisible_subset_result_);
end;
procedure main();
var
  main_items: array of integer;
  main_subset: IntArray;
begin
  main_items := [1, 16, 7, 8, 4];
  main_subset := largest_divisible_subset(main_items);
  writeln(((('The longest divisible subset of ' + list_int_to_str(main_items)) + ' is ') + list_int_to_str(main_subset)) + '.');
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
