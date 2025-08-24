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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function merge(merge_a: IntArray; merge_low: int64; merge_mid: int64; merge_high: int64): IntArray; forward;
function iter_merge_sort(iter_merge_sort_items: IntArray): IntArray; forward;
function list_to_string(list_to_string_arr: IntArray): string; forward;
function merge(merge_a: IntArray; merge_low: int64; merge_mid: int64; merge_high: int64): IntArray;
var
  merge_left: array of int64;
  merge_right: array of int64;
  merge_result_: array of int64;
  merge_i: int64;
begin
  merge_left := copy(merge_a, merge_low, (merge_mid - (merge_low)));
  merge_right := copy(merge_a, merge_mid, (merge_high + 1 - (merge_mid)));
  merge_result_ := [];
  while (Length(merge_left) > 0) and (Length(merge_right) > 0) do begin
  if merge_left[0] <= merge_right[0] then begin
  merge_result_ := concat(merge_result_, IntArray([merge_left[0]]));
  merge_left := copy(merge_left, 1, Length(merge_left));
end else begin
  merge_result_ := concat(merge_result_, IntArray([merge_right[0]]));
  merge_right := copy(merge_right, 1, Length(merge_right));
end;
end;
  merge_i := 0;
  while merge_i < Length(merge_left) do begin
  merge_result_ := concat(merge_result_, IntArray([merge_left[merge_i]]));
  merge_i := merge_i + 1;
end;
  merge_i := 0;
  while merge_i < Length(merge_right) do begin
  merge_result_ := concat(merge_result_, IntArray([merge_right[merge_i]]));
  merge_i := merge_i + 1;
end;
  merge_i := 0;
  while merge_i < Length(merge_result_) do begin
  merge_a[merge_low + merge_i] := merge_result_[merge_i];
  merge_i := merge_i + 1;
end;
  exit(merge_a);
end;
function iter_merge_sort(iter_merge_sort_items: IntArray): IntArray;
var
  iter_merge_sort_n: integer;
  iter_merge_sort_arr: array of int64;
  iter_merge_sort_p: int64;
  iter_merge_sort_i: int64;
  iter_merge_sort_high: int64;
  iter_merge_sort_low: int64;
  iter_merge_sort_mid: int64;
  iter_merge_sort_mid2: int64;
begin
  iter_merge_sort_n := Length(iter_merge_sort_items);
  if iter_merge_sort_n <= 1 then begin
  exit(iter_merge_sort_items);
end;
  iter_merge_sort_arr := copy(iter_merge_sort_items, 0, Length(iter_merge_sort_items));
  iter_merge_sort_p := 2;
  while iter_merge_sort_p <= iter_merge_sort_n do begin
  iter_merge_sort_i := 0;
  while iter_merge_sort_i < iter_merge_sort_n do begin
  iter_merge_sort_high := (iter_merge_sort_i + iter_merge_sort_p) - 1;
  if iter_merge_sort_high >= iter_merge_sort_n then begin
  iter_merge_sort_high := iter_merge_sort_n - 1;
end;
  iter_merge_sort_low := iter_merge_sort_i;
  iter_merge_sort_mid := ((iter_merge_sort_low + iter_merge_sort_high) + 1) div 2;
  iter_merge_sort_arr := merge(iter_merge_sort_arr, iter_merge_sort_low, iter_merge_sort_mid, iter_merge_sort_high);
  iter_merge_sort_i := iter_merge_sort_i + iter_merge_sort_p;
end;
  if (iter_merge_sort_p * 2) >= iter_merge_sort_n then begin
  iter_merge_sort_mid2 := iter_merge_sort_i - iter_merge_sort_p;
  iter_merge_sort_arr := merge(iter_merge_sort_arr, 0, iter_merge_sort_mid2, iter_merge_sort_n - 1);
  break;
end;
  iter_merge_sort_p := iter_merge_sort_p * 2;
end;
  exit(iter_merge_sort_arr);
end;
function list_to_string(list_to_string_arr: IntArray): string;
var
  list_to_string_s: string;
  list_to_string_i: int64;
begin
  list_to_string_s := '[';
  list_to_string_i := 0;
  while list_to_string_i < Length(list_to_string_arr) do begin
  list_to_string_s := list_to_string_s + IntToStr(list_to_string_arr[list_to_string_i]);
  if list_to_string_i < (Length(list_to_string_arr) - 1) then begin
  list_to_string_s := list_to_string_s + ', ';
end;
  list_to_string_i := list_to_string_i + 1;
end;
  exit(list_to_string_s + ']');
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_to_string(iter_merge_sort([5, 9, 8, 7, 1, 2, 7])));
  writeln(list_to_string(iter_merge_sort([1])));
  writeln(list_to_string(iter_merge_sort([2, 1])));
  writeln(list_to_string(iter_merge_sort([4, 3, 2, 1])));
  writeln(list_to_string(iter_merge_sort([5, 4, 3, 2, 1])));
  writeln(list_to_string(iter_merge_sort([-2, -9, -1, -4])));
  writeln(list_to_string(iter_merge_sort([])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
