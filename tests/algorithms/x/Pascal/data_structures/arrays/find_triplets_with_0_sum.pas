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
  c: integer;
  a: integer;
  nums: IntArray;
  b: integer;
  target: IntArray;
  value: integer;
  arr: IntArray;
function sort_triplet(a: integer; b: integer; c: integer): IntArray; forward;
function contains_triplet(arr: IntArrayArray; target: IntArray): boolean; forward;
function contains_int(arr: IntArray; value: integer): boolean; forward;
function find_triplets_with_0_sum(nums: IntArray): IntArrayArray; forward;
function find_triplets_with_0_sum_hashing(arr: IntArray): IntArrayArray; forward;
function sort_triplet(a: integer; b: integer; c: integer): IntArray;
var
  sort_triplet_x: integer;
  sort_triplet_y: integer;
  sort_triplet_z: integer;
  sort_triplet_t: integer;
begin
  sort_triplet_x := a;
  sort_triplet_y := b;
  sort_triplet_z := c;
  if sort_triplet_x > sort_triplet_y then begin
  sort_triplet_t := sort_triplet_x;
  sort_triplet_x := sort_triplet_y;
  sort_triplet_y := sort_triplet_t;
end;
  if sort_triplet_y > sort_triplet_z then begin
  sort_triplet_t := sort_triplet_y;
  sort_triplet_y := sort_triplet_z;
  sort_triplet_z := sort_triplet_t;
end;
  if sort_triplet_x > sort_triplet_y then begin
  sort_triplet_t := sort_triplet_x;
  sort_triplet_x := sort_triplet_y;
  sort_triplet_y := sort_triplet_t;
end;
  exit([sort_triplet_x, sort_triplet_y, sort_triplet_z]);
end;
function contains_triplet(arr: IntArrayArray; target: IntArray): boolean;
var
  contains_triplet_i: int64;
  contains_triplet_item: array of integer;
  contains_triplet_same: boolean;
  contains_triplet_j: int64;
begin
  for contains_triplet_i := 0 to (Length(arr) - 1) do begin
  contains_triplet_item := arr[contains_triplet_i];
  contains_triplet_same := true;
  for contains_triplet_j := 0 to (Length(target) - 1) do begin
  if contains_triplet_item[contains_triplet_j] <> target[contains_triplet_j] then begin
  contains_triplet_same := false;
  break;
end;
end;
  if contains_triplet_same then begin
  exit(true);
end;
end;
  exit(false);
end;
function contains_int(arr: IntArray; value: integer): boolean;
var
  contains_int_i: int64;
begin
  for contains_int_i := 0 to (Length(arr) - 1) do begin
  if arr[contains_int_i] = value then begin
  exit(true);
end;
end;
  exit(false);
end;
function find_triplets_with_0_sum(nums: IntArray): IntArrayArray;
var
  find_triplets_with_0_sum_n: integer;
  find_triplets_with_0_sum_result_: array of IntArray;
  find_triplets_with_0_sum_i: int64;
  find_triplets_with_0_sum_j: int64;
  find_triplets_with_0_sum_k: int64;
  find_triplets_with_0_sum_a: integer;
  find_triplets_with_0_sum_b: integer;
  find_triplets_with_0_sum_c: integer;
  find_triplets_with_0_sum_trip: IntArray;
begin
  find_triplets_with_0_sum_n := Length(nums);
  find_triplets_with_0_sum_result_ := [];
  for find_triplets_with_0_sum_i := 0 to (find_triplets_with_0_sum_n - 1) do begin
  for find_triplets_with_0_sum_j := find_triplets_with_0_sum_i + 1 to (find_triplets_with_0_sum_n - 1) do begin
  for find_triplets_with_0_sum_k := find_triplets_with_0_sum_j + 1 to (find_triplets_with_0_sum_n - 1) do begin
  find_triplets_with_0_sum_a := nums[find_triplets_with_0_sum_i];
  find_triplets_with_0_sum_b := nums[find_triplets_with_0_sum_j];
  find_triplets_with_0_sum_c := nums[find_triplets_with_0_sum_k];
  if ((find_triplets_with_0_sum_a + find_triplets_with_0_sum_b) + find_triplets_with_0_sum_c) = 0 then begin
  find_triplets_with_0_sum_trip := sort_triplet(find_triplets_with_0_sum_a, find_triplets_with_0_sum_b, find_triplets_with_0_sum_c);
  if not contains_triplet(find_triplets_with_0_sum_result_, find_triplets_with_0_sum_trip) then begin
  find_triplets_with_0_sum_result_ := concat(find_triplets_with_0_sum_result_, [find_triplets_with_0_sum_trip]);
end;
end;
end;
end;
end;
  exit(find_triplets_with_0_sum_result_);
end;
function find_triplets_with_0_sum_hashing(arr: IntArray): IntArrayArray;
var
  find_triplets_with_0_sum_hashing_target_sum: integer;
  find_triplets_with_0_sum_hashing_output: array of IntArray;
  find_triplets_with_0_sum_hashing_i: int64;
  find_triplets_with_0_sum_hashing_seen: array of integer;
  find_triplets_with_0_sum_hashing_current_sum: integer;
  find_triplets_with_0_sum_hashing_j: int64;
  find_triplets_with_0_sum_hashing_other: integer;
  find_triplets_with_0_sum_hashing_required: integer;
  find_triplets_with_0_sum_hashing_trip: IntArray;
begin
  find_triplets_with_0_sum_hashing_target_sum := 0;
  find_triplets_with_0_sum_hashing_output := [];
  for find_triplets_with_0_sum_hashing_i := 0 to (Length(arr) - 1) do begin
  find_triplets_with_0_sum_hashing_seen := [];
  find_triplets_with_0_sum_hashing_current_sum := find_triplets_with_0_sum_hashing_target_sum - arr[find_triplets_with_0_sum_hashing_i];
  for find_triplets_with_0_sum_hashing_j := find_triplets_with_0_sum_hashing_i + 1 to (Length(arr) - 1) do begin
  find_triplets_with_0_sum_hashing_other := arr[find_triplets_with_0_sum_hashing_j];
  find_triplets_with_0_sum_hashing_required := find_triplets_with_0_sum_hashing_current_sum - find_triplets_with_0_sum_hashing_other;
  if contains_int(find_triplets_with_0_sum_hashing_seen, find_triplets_with_0_sum_hashing_required) then begin
  find_triplets_with_0_sum_hashing_trip := sort_triplet(arr[find_triplets_with_0_sum_hashing_i], find_triplets_with_0_sum_hashing_other, find_triplets_with_0_sum_hashing_required);
  if not contains_triplet(find_triplets_with_0_sum_hashing_output, find_triplets_with_0_sum_hashing_trip) then begin
  find_triplets_with_0_sum_hashing_output := concat(find_triplets_with_0_sum_hashing_output, [find_triplets_with_0_sum_hashing_trip]);
end;
end;
  find_triplets_with_0_sum_hashing_seen := concat(find_triplets_with_0_sum_hashing_seen, IntArray([find_triplets_with_0_sum_hashing_other]));
end;
end;
  exit(find_triplets_with_0_sum_hashing_output);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_list_int_to_str(find_triplets_with_0_sum([-1, 0, 1, 2, -1, -4])));
  writeln(list_list_int_to_str(find_triplets_with_0_sum([])));
  writeln(list_list_int_to_str(find_triplets_with_0_sum([0, 0, 0])));
  writeln(list_list_int_to_str(find_triplets_with_0_sum([1, 2, 3, 0, -1, -2, -3])));
  writeln(list_list_int_to_str(find_triplets_with_0_sum_hashing([-1, 0, 1, 2, -1, -4])));
  writeln(list_list_int_to_str(find_triplets_with_0_sum_hashing([])));
  writeln(list_list_int_to_str(find_triplets_with_0_sum_hashing([0, 0, 0])));
  writeln(list_list_int_to_str(find_triplets_with_0_sum_hashing([1, 2, 3, 0, -1, -2, -3])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
