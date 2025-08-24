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
function list_min(list_min_xs: IntArray): int64; forward;
function list_max(list_max_xs: IntArray): int64; forward;
function remove_once(remove_once_xs: IntArray; remove_once_value: int64): IntArray; forward;
function reverse_list(reverse_list_xs: IntArray): IntArray; forward;
function merge_sort(merge_sort_collection: IntArray): IntArray; forward;
procedure test_merge_sort(); forward;
procedure main(); forward;
function list_min(list_min_xs: IntArray): int64;
var
  list_min_i: int64;
  list_min_m: int64;
begin
  list_min_i := 1;
  list_min_m := list_min_xs[0];
  while list_min_i < Length(list_min_xs) do begin
  if list_min_xs[list_min_i] < list_min_m then begin
  list_min_m := list_min_xs[list_min_i];
end;
  list_min_i := list_min_i + 1;
end;
  exit(list_min_m);
end;
function list_max(list_max_xs: IntArray): int64;
var
  list_max_i: int64;
  list_max_m: int64;
begin
  list_max_i := 1;
  list_max_m := list_max_xs[0];
  while list_max_i < Length(list_max_xs) do begin
  if list_max_xs[list_max_i] > list_max_m then begin
  list_max_m := list_max_xs[list_max_i];
end;
  list_max_i := list_max_i + 1;
end;
  exit(list_max_m);
end;
function remove_once(remove_once_xs: IntArray; remove_once_value: int64): IntArray;
var
  remove_once_res: array of int64;
  remove_once_removed: boolean;
  remove_once_i: int64;
begin
  remove_once_res := [];
  remove_once_removed := false;
  remove_once_i := 0;
  while remove_once_i < Length(remove_once_xs) do begin
  if not remove_once_removed and (remove_once_xs[remove_once_i] = remove_once_value) then begin
  remove_once_removed := true;
end else begin
  remove_once_res := concat(remove_once_res, IntArray([remove_once_xs[remove_once_i]]));
end;
  remove_once_i := remove_once_i + 1;
end;
  exit(remove_once_res);
end;
function reverse_list(reverse_list_xs: IntArray): IntArray;
var
  reverse_list_res: array of int64;
  reverse_list_i: integer;
begin
  reverse_list_res := [];
  reverse_list_i := Length(reverse_list_xs) - 1;
  while reverse_list_i >= 0 do begin
  reverse_list_res := concat(reverse_list_res, IntArray([reverse_list_xs[reverse_list_i]]));
  reverse_list_i := reverse_list_i - 1;
end;
  exit(reverse_list_res);
end;
function merge_sort(merge_sort_collection: IntArray): IntArray;
var
  merge_sort_start: array of int64;
  merge_sort_end_: array of int64;
  merge_sort_coll: array of int64;
  merge_sort_mn: int64;
  merge_sort_mx: int64;
begin
  merge_sort_start := [];
  merge_sort_end_ := [];
  merge_sort_coll := merge_sort_collection;
  while Length(merge_sort_coll) > 1 do begin
  merge_sort_mn := list_min(merge_sort_coll);
  merge_sort_mx := list_max(merge_sort_coll);
  merge_sort_start := concat(merge_sort_start, IntArray([merge_sort_mn]));
  merge_sort_end_ := concat(merge_sort_end_, IntArray([merge_sort_mx]));
  merge_sort_coll := remove_once(merge_sort_coll, merge_sort_mn);
  merge_sort_coll := remove_once(merge_sort_coll, merge_sort_mx);
end;
  merge_sort_end_ := reverse_list(merge_sort_end_);
  exit(concat(concat(merge_sort_start, merge_sort_coll), merge_sort_end_));
end;
procedure test_merge_sort();
begin
  if list_int_to_str(merge_sort([0, 5, 3, 2, 2])) <> list_int_to_str([0, 2, 2, 3, 5]) then begin
  panic('case1 failed');
end;
  if list_int_to_str(merge_sort([])) <> list_int_to_str([]) then begin
  panic('case2 failed');
end;
  if list_int_to_str(merge_sort([-2, -5, -45])) <> list_int_to_str([-45, -5, -2]) then begin
  panic('case3 failed');
end;
end;
procedure main();
begin
  test_merge_sort();
  writeln(list_int_to_str(merge_sort([0, 5, 3, 2, 2])));
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
  writeln('');
end.
