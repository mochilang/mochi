{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64);
begin
  writeln(x);
end;
function list_int_to_str(xs: array of int64): string;
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
function subarray(subarray_xs: IntArray; subarray_start: int64; subarray_end_: int64): IntArray; forward;
function merge(merge_left: IntArray; merge_right: IntArray): IntArray; forward;
function merge_sort(merge_sort_collection: IntArray): IntArray; forward;
function subarray(subarray_xs: IntArray; subarray_start: int64; subarray_end_: int64): IntArray;
var
  subarray_result_: array of int64;
  subarray_i: int64;
begin
  subarray_result_ := [];
  subarray_i := subarray_start;
  while subarray_i < subarray_end_ do begin
  subarray_result_ := concat(subarray_result_, IntArray([subarray_xs[subarray_i]]));
  subarray_i := subarray_i + 1;
end;
  exit(subarray_result_);
end;
function merge(merge_left: IntArray; merge_right: IntArray): IntArray;
var
  merge_result_: array of int64;
  merge_i: int64;
  merge_j: int64;
begin
  merge_result_ := [];
  merge_i := 0;
  merge_j := 0;
  while (merge_i < Length(merge_left)) and (merge_j < Length(merge_right)) do begin
  if merge_left[merge_i] <= merge_right[merge_j] then begin
  merge_result_ := concat(merge_result_, IntArray([merge_left[merge_i]]));
  merge_i := merge_i + 1;
end else begin
  merge_result_ := concat(merge_result_, IntArray([merge_right[merge_j]]));
  merge_j := merge_j + 1;
end;
end;
  while merge_i < Length(merge_left) do begin
  merge_result_ := concat(merge_result_, IntArray([merge_left[merge_i]]));
  merge_i := merge_i + 1;
end;
  while merge_j < Length(merge_right) do begin
  merge_result_ := concat(merge_result_, IntArray([merge_right[merge_j]]));
  merge_j := merge_j + 1;
end;
  exit(merge_result_);
end;
function merge_sort(merge_sort_collection: IntArray): IntArray;
var
  merge_sort_mid_index: integer;
  merge_sort_left: IntArray;
  merge_sort_right: IntArray;
  merge_sort_sorted_left: array of int64;
  merge_sort_sorted_right: array of int64;
begin
  if Length(merge_sort_collection) <= 1 then begin
  exit(merge_sort_collection);
end;
  merge_sort_mid_index := Length(merge_sort_collection) div 2;
  merge_sort_left := subarray(merge_sort_collection, 0, merge_sort_mid_index);
  merge_sort_right := subarray(merge_sort_collection, merge_sort_mid_index, Length(merge_sort_collection));
  merge_sort_sorted_left := merge_sort(merge_sort_left);
  merge_sort_sorted_right := merge_sort(merge_sort_right);
  exit(merge(merge_sort_sorted_left, merge_sort_sorted_right));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(merge_sort([0, 5, 3, 2, 2])));
  writeln(list_int_to_str(merge_sort([])));
  writeln(list_int_to_str(merge_sort([-2, -5, -45])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
