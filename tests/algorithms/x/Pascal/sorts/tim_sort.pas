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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  sample: array of int64;
  sorted_sample: IntArray;
  sample2: array of int64;
  sorted_sample2: IntArray;
function copy_list(copy_list_xs: IntArray): IntArray; forward;
function insertion_sort(insertion_sort_xs: IntArray): IntArray; forward;
function merge(merge_left: IntArray; merge_right: IntArray): IntArray; forward;
function tim_sort(tim_sort_xs: IntArray): IntArray; forward;
function list_to_string(list_to_string_xs: IntArray): string; forward;
function copy_list(copy_list_xs: IntArray): IntArray;
var
  copy_list_res: array of int64;
  copy_list_k: int64;
begin
  copy_list_res := [];
  copy_list_k := 0;
  while copy_list_k < Length(copy_list_xs) do begin
  copy_list_res := concat(copy_list_res, IntArray([copy_list_xs[copy_list_k]]));
  copy_list_k := copy_list_k + 1;
end;
  exit(copy_list_res);
end;
function insertion_sort(insertion_sort_xs: IntArray): IntArray;
var
  insertion_sort_arr: IntArray;
  insertion_sort_idx: int64;
  insertion_sort_value: int64;
  insertion_sort_jdx: int64;
begin
  insertion_sort_arr := copy_list(insertion_sort_xs);
  insertion_sort_idx := 1;
  while insertion_sort_idx < Length(insertion_sort_arr) do begin
  insertion_sort_value := insertion_sort_arr[insertion_sort_idx];
  insertion_sort_jdx := insertion_sort_idx - 1;
  while (insertion_sort_jdx >= 0) and (insertion_sort_arr[insertion_sort_jdx] > insertion_sort_value) do begin
  insertion_sort_arr[insertion_sort_jdx + 1] := insertion_sort_arr[insertion_sort_jdx];
  insertion_sort_jdx := insertion_sort_jdx - 1;
end;
  insertion_sort_arr[insertion_sort_jdx + 1] := insertion_sort_value;
  insertion_sort_idx := insertion_sort_idx + 1;
end;
  exit(insertion_sort_arr);
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
  if merge_left[merge_i] < merge_right[merge_j] then begin
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
function tim_sort(tim_sort_xs: IntArray): IntArray;
var
  tim_sort_n: integer;
  tim_sort_runs: array of IntArray;
  tim_sort_sorted_runs: array of IntArray;
  tim_sort_current: array of int64;
  tim_sort_i: int64;
  tim_sort_r: int64;
  tim_sort_result_: array of int64;
begin
  tim_sort_n := Length(tim_sort_xs);
  tim_sort_runs := [];
  tim_sort_sorted_runs := [];
  tim_sort_current := [];
  tim_sort_current := concat(tim_sort_current, IntArray([tim_sort_xs[0]]));
  tim_sort_i := 1;
  while tim_sort_i < tim_sort_n do begin
  if tim_sort_xs[tim_sort_i] < tim_sort_xs[tim_sort_i - 1] then begin
  tim_sort_runs := concat(tim_sort_runs, [copy_list(tim_sort_current)]);
  tim_sort_current := [];
  tim_sort_current := concat(tim_sort_current, IntArray([tim_sort_xs[tim_sort_i]]));
end else begin
  tim_sort_current := concat(tim_sort_current, IntArray([tim_sort_xs[tim_sort_i]]));
end;
  tim_sort_i := tim_sort_i + 1;
end;
  tim_sort_runs := concat(tim_sort_runs, [copy_list(tim_sort_current)]);
  tim_sort_r := 0;
  while tim_sort_r < Length(tim_sort_runs) do begin
  tim_sort_sorted_runs := concat(tim_sort_sorted_runs, [insertion_sort(tim_sort_runs[tim_sort_r])]);
  tim_sort_r := tim_sort_r + 1;
end;
  tim_sort_result_ := [];
  tim_sort_r := 0;
  while tim_sort_r < Length(tim_sort_sorted_runs) do begin
  tim_sort_result_ := merge(tim_sort_result_, tim_sort_sorted_runs[tim_sort_r]);
  tim_sort_r := tim_sort_r + 1;
end;
  exit(tim_sort_result_);
end;
function list_to_string(list_to_string_xs: IntArray): string;
var
  list_to_string_s: string;
  list_to_string_k: int64;
begin
  list_to_string_s := '[';
  list_to_string_k := 0;
  while list_to_string_k < Length(list_to_string_xs) do begin
  list_to_string_s := list_to_string_s + IntToStr(list_to_string_xs[list_to_string_k]);
  if list_to_string_k < (Length(list_to_string_xs) - 1) then begin
  list_to_string_s := list_to_string_s + ', ';
end;
  list_to_string_k := list_to_string_k + 1;
end;
  exit(list_to_string_s + ']');
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  sample := [5, 9, 10, 3, -4, 5, 178, 92, 46, -18, 0, 7];
  sorted_sample := tim_sort(sample);
  writeln(list_to_string(sorted_sample));
  sample2 := [3, 2, 1];
  sorted_sample2 := tim_sort(sample2);
  writeln(list_to_string(sorted_sample2));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
