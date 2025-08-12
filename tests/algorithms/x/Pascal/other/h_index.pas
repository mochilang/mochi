{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of integer;
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
  array_: IntArray;
  start: integer;
  right_half: IntArray;
  end_: integer;
  citations: IntArray;
  xs: IntArray;
  left_half: IntArray;
function subarray(xs: IntArray; start: integer; end_: integer): IntArray; forward;
function merge(left_half: IntArray; right_half: IntArray): IntArray; forward;
function merge_sort(array_: IntArray): IntArray; forward;
function h_index(citations: IntArray): integer; forward;
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
function h_index(citations: IntArray): integer;
var
  h_index_idx: integer;
  h_index_sorted: IntArray;
  h_index_n: integer;
  h_index_i: integer;
begin
  h_index_idx := 0;
  while h_index_idx < Length(citations) do begin
  if citations[h_index_idx] < 0 then begin
  panic('The citations should be a list of non negative integers.');
end;
  h_index_idx := h_index_idx + 1;
end;
  h_index_sorted := merge_sort(citations);
  h_index_n := Length(h_index_sorted);
  h_index_i := 0;
  while h_index_i < h_index_n do begin
  if h_index_sorted[(h_index_n - 1) - h_index_i] <= h_index_i then begin
  exit(h_index_i);
end;
  h_index_i := h_index_i + 1;
end;
  exit(h_index_n);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(h_index([3, 0, 6, 1, 5])));
  writeln(IntToStr(h_index([1, 3, 1])));
  writeln(IntToStr(h_index([1, 2, 3])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
