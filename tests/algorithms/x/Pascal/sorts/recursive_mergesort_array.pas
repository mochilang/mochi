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
function merge(merge_arr: IntArray): IntArray; forward;
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
function merge(merge_arr: IntArray): IntArray;
var
  merge_middle_length: integer;
  merge_left_array: IntArray;
  merge_right_array: IntArray;
  merge_left_size: integer;
  merge_right_size: integer;
  merge_left_index: int64;
  merge_right_index: int64;
  merge_index: int64;
begin
  if Length(merge_arr) > 1 then begin
  merge_middle_length := Length(merge_arr) div 2;
  merge_left_array := subarray(merge_arr, 0, merge_middle_length);
  merge_right_array := subarray(merge_arr, merge_middle_length, Length(merge_arr));
  merge_left_size := Length(merge_left_array);
  merge_right_size := Length(merge_right_array);
  merge(merge_left_array);
  merge(merge_right_array);
  merge_left_index := 0;
  merge_right_index := 0;
  merge_index := 0;
  while (merge_left_index < merge_left_size) and (merge_right_index < merge_right_size) do begin
  if merge_left_array[merge_left_index] < merge_right_array[merge_right_index] then begin
  merge_arr[merge_index] := merge_left_array[merge_left_index];
  merge_left_index := merge_left_index + 1;
end else begin
  merge_arr[merge_index] := merge_right_array[merge_right_index];
  merge_right_index := merge_right_index + 1;
end;
  merge_index := merge_index + 1;
end;
  while merge_left_index < merge_left_size do begin
  merge_arr[merge_index] := merge_left_array[merge_left_index];
  merge_left_index := merge_left_index + 1;
  merge_index := merge_index + 1;
end;
  while merge_right_index < merge_right_size do begin
  merge_arr[merge_index] := merge_right_array[merge_right_index];
  merge_right_index := merge_right_index + 1;
  merge_index := merge_index + 1;
end;
end;
  exit(merge_arr);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(merge([10, 9, 8, 7, 6, 5, 4, 3, 2, 1])));
  writeln(list_int_to_str(merge([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])));
  writeln(list_int_to_str(merge([10, 22, 1, 2, 3, 9, 15, 23])));
  writeln(list_int_to_str(merge([100])));
  writeln(list_int_to_str(merge([])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
