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
  array_: IntArray;
  left_half: IntArray;
  xs: IntArray;
  end_: integer;
  right_half: IntArray;
function subarray(xs: IntArray; start: integer; end_: integer): IntArray; forward;
function merge(left_half: IntArray; right_half: IntArray): IntArray; forward;
function merge_sort(array_: IntArray): IntArray; forward;
function subarray(xs: IntArray; start: integer; end_: integer): IntArray;
var
  subarray_result_: array of integer;
  subarray_k: integer;
begin
  subarray_result_ := [];
  subarray_k := start;
  while subarray_k < end_ do begin
  subarray_result_ := concat(subarray_result_, IntArray([xs[subarray_k]]));
  subarray_k := subarray_k + 1;
end;
  exit(subarray_result_);
end;
function merge(left_half: IntArray; right_half: IntArray): IntArray;
var
  merge_result_: array of integer;
  merge_i: integer;
  merge_j: integer;
begin
  merge_result_ := [];
  merge_i := 0;
  merge_j := 0;
  while (merge_i < Length(left_half)) and (merge_j < Length(right_half)) do begin
  if left_half[merge_i] < right_half[merge_j] then begin
  merge_result_ := concat(merge_result_, IntArray([left_half[merge_i]]));
  merge_i := merge_i + 1;
end else begin
  merge_result_ := concat(merge_result_, IntArray([right_half[merge_j]]));
  merge_j := merge_j + 1;
end;
end;
  while merge_i < Length(left_half) do begin
  merge_result_ := concat(merge_result_, IntArray([left_half[merge_i]]));
  merge_i := merge_i + 1;
end;
  while merge_j < Length(right_half) do begin
  merge_result_ := concat(merge_result_, IntArray([right_half[merge_j]]));
  merge_j := merge_j + 1;
end;
  exit(merge_result_);
end;
function merge_sort(array_: IntArray): IntArray;
var
  merge_sort_middle: integer;
  merge_sort_left_half: IntArray;
  merge_sort_right_half: IntArray;
  merge_sort_sorted_left: array of integer;
  merge_sort_sorted_right: array of integer;
begin
  if Length(array_) <= 1 then begin
  exit(array_);
end;
  merge_sort_middle := Length(array_) div 2;
  merge_sort_left_half := subarray(array_, 0, merge_sort_middle);
  merge_sort_right_half := subarray(array_, merge_sort_middle, Length(array_));
  merge_sort_sorted_left := merge_sort(merge_sort_left_half);
  merge_sort_sorted_right := merge_sort(merge_sort_right_half);
  exit(merge(merge_sort_sorted_left, merge_sort_sorted_right));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(merge_sort([5, 3, 1, 4, 2])));
  writeln(list_int_to_str(merge_sort([-2, 3, -10, 11, 99, 100000, 100, -200])));
  writeln(list_int_to_str(merge_sort([-200])));
  writeln(list_int_to_str(merge_sort([])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
