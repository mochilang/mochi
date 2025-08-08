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
  start: integer;
  output: IntArrayArray;
  xs: IntArray;
  nums: IntArray;
  j: integer;
  i: integer;
function tail(xs: IntArray): IntArray; forward;
function rotate_left(xs: IntArray): IntArray; forward;
function permute_recursive(nums: IntArray): IntArrayArray; forward;
function swap(xs: IntArray; i: integer; j: integer): IntArray; forward;
function permute_backtrack_helper(nums: IntArray; start: integer; output: IntArrayArray): IntArrayArray; forward;
function permute_backtrack(nums: IntArray): IntArrayArray; forward;
function tail(xs: IntArray): IntArray;
var
  tail_res: array of integer;
  tail_i: integer;
begin
  tail_res := [];
  tail_i := 1;
  while tail_i < Length(xs) do begin
  tail_res := concat(tail_res, IntArray([xs[tail_i]]));
  tail_i := tail_i + 1;
end;
  exit(tail_res);
end;
function rotate_left(xs: IntArray): IntArray;
var
  rotate_left_res: array of integer;
  rotate_left_i: integer;
begin
  if Length(xs) = 0 then begin
  exit(xs);
end;
  rotate_left_res := [];
  rotate_left_i := 1;
  while rotate_left_i < Length(xs) do begin
  rotate_left_res := concat(rotate_left_res, IntArray([xs[rotate_left_i]]));
  rotate_left_i := rotate_left_i + 1;
end;
  rotate_left_res := concat(rotate_left_res, IntArray([xs[0]]));
  exit(rotate_left_res);
end;
function permute_recursive(nums: IntArray): IntArrayArray;
var
  permute_recursive_base: array of IntArray;
  permute_recursive_result_: array of IntArray;
  permute_recursive_current: array of integer;
  permute_recursive_count: integer;
  permute_recursive_n: integer;
  permute_recursive_rest: IntArray;
  permute_recursive_perms: array of IntArray;
  permute_recursive_j: integer;
  permute_recursive_perm: array of integer;
begin
  if Length(nums) = 0 then begin
  permute_recursive_base := [];
  exit(concat(permute_recursive_base, [IntArray([])]));
end;
  permute_recursive_result_ := [];
  permute_recursive_current := nums;
  permute_recursive_count := 0;
  while permute_recursive_count < Length(nums) do begin
  permute_recursive_n := permute_recursive_current[0];
  permute_recursive_rest := tail(permute_recursive_current);
  permute_recursive_perms := permute_recursive(permute_recursive_rest);
  permute_recursive_j := 0;
  while permute_recursive_j < Length(permute_recursive_perms) do begin
  permute_recursive_perm := concat(permute_recursive_perms[permute_recursive_j], IntArray([permute_recursive_n]));
  permute_recursive_result_ := concat(permute_recursive_result_, [permute_recursive_perm]);
  permute_recursive_j := permute_recursive_j + 1;
end;
  permute_recursive_current := rotate_left(permute_recursive_current);
  permute_recursive_count := permute_recursive_count + 1;
end;
  exit(permute_recursive_result_);
end;
function swap(xs: IntArray; i: integer; j: integer): IntArray;
var
  swap_res: array of integer;
  swap_k: integer;
begin
  swap_res := [];
  swap_k := 0;
  while swap_k < Length(xs) do begin
  if swap_k = i then begin
  swap_res := concat(swap_res, IntArray([xs[j]]));
end else begin
  if swap_k = j then begin
  swap_res := concat(swap_res, IntArray([xs[i]]));
end else begin
  swap_res := concat(swap_res, IntArray([xs[swap_k]]));
end;
end;
  swap_k := swap_k + 1;
end;
  exit(swap_res);
end;
function permute_backtrack_helper(nums: IntArray; start: integer; output: IntArrayArray): IntArrayArray;
var
  permute_backtrack_helper_i: integer;
  permute_backtrack_helper_res: array of IntArray;
  permute_backtrack_helper_swapped: IntArray;
begin
  if start = (Length(nums) - 1) then begin
  exit(concat(output, [nums]));
end;
  permute_backtrack_helper_i := start;
  permute_backtrack_helper_res := output;
  while permute_backtrack_helper_i < Length(nums) do begin
  permute_backtrack_helper_swapped := swap(nums, start, permute_backtrack_helper_i);
  permute_backtrack_helper_res := permute_backtrack_helper(permute_backtrack_helper_swapped, start + 1, permute_backtrack_helper_res);
  permute_backtrack_helper_i := permute_backtrack_helper_i + 1;
end;
  exit(permute_backtrack_helper_res);
end;
function permute_backtrack(nums: IntArray): IntArrayArray;
var
  permute_backtrack_output: array of IntArray;
begin
  permute_backtrack_output := [];
  exit(permute_backtrack_helper(nums, 0, permute_backtrack_output));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_list_int_to_str(permute_recursive([1, 2, 3])));
  writeln(list_list_int_to_str(permute_backtrack([1, 2, 3])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
