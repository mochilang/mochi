{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function subarray(subarray_xs: IntArray; subarray_start: int64; subarray_end_: int64): IntArray; forward;
function merge(merge_left_half: IntArray; merge_right_half: IntArray): IntArray; forward;
function merge_sort(merge_sort_array_: IntArray): IntArray; forward;
function h_index(h_index_citations: IntArray): int64; forward;
function subarray(subarray_xs: IntArray; subarray_start: int64; subarray_end_: int64): IntArray;
var
  subarray_result_: array of int64;
  subarray_k: int64;
begin
  subarray_result_ := [];
  subarray_k := subarray_start;
  while subarray_k < subarray_end_ do begin
  subarray_result_ := concat(subarray_result_, IntArray([subarray_xs[subarray_k]]));
  subarray_k := subarray_k + 1;
end;
  exit(subarray_result_);
end;
function merge(merge_left_half: IntArray; merge_right_half: IntArray): IntArray;
var
  merge_result_: array of int64;
  merge_i: int64;
  merge_j: int64;
begin
  merge_result_ := [];
  merge_i := 0;
  merge_j := 0;
  while (merge_i < Length(merge_left_half)) and (merge_j < Length(merge_right_half)) do begin
  if merge_left_half[merge_i] < merge_right_half[merge_j] then begin
  merge_result_ := concat(merge_result_, IntArray([merge_left_half[merge_i]]));
  merge_i := merge_i + 1;
end else begin
  merge_result_ := concat(merge_result_, IntArray([merge_right_half[merge_j]]));
  merge_j := merge_j + 1;
end;
end;
  while merge_i < Length(merge_left_half) do begin
  merge_result_ := concat(merge_result_, IntArray([merge_left_half[merge_i]]));
  merge_i := merge_i + 1;
end;
  while merge_j < Length(merge_right_half) do begin
  merge_result_ := concat(merge_result_, IntArray([merge_right_half[merge_j]]));
  merge_j := merge_j + 1;
end;
  exit(merge_result_);
end;
function merge_sort(merge_sort_array_: IntArray): IntArray;
var
  merge_sort_middle: integer;
  merge_sort_left_half: IntArray;
  merge_sort_right_half: IntArray;
  merge_sort_sorted_left: array of int64;
  merge_sort_sorted_right: array of int64;
begin
  if Length(merge_sort_array_) <= 1 then begin
  exit(merge_sort_array_);
end;
  merge_sort_middle := _floordiv(Length(merge_sort_array_), 2);
  merge_sort_left_half := subarray(merge_sort_array_, 0, merge_sort_middle);
  merge_sort_right_half := subarray(merge_sort_array_, merge_sort_middle, Length(merge_sort_array_));
  merge_sort_sorted_left := merge_sort(merge_sort_left_half);
  merge_sort_sorted_right := merge_sort(merge_sort_right_half);
  exit(merge(merge_sort_sorted_left, merge_sort_sorted_right));
end;
function h_index(h_index_citations: IntArray): int64;
var
  h_index_idx: int64;
  h_index_sorted: IntArray;
  h_index_n: integer;
  h_index_i: int64;
begin
  h_index_idx := 0;
  while h_index_idx < Length(h_index_citations) do begin
  if h_index_citations[h_index_idx] < 0 then begin
  panic('The citations should be a list of non negative integers.');
end;
  h_index_idx := h_index_idx + 1;
end;
  h_index_sorted := merge_sort(h_index_citations);
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
  writeln('');
end.
