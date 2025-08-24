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
function binary_search_insertion_from(binary_search_insertion_from_sorted_list: IntArray; binary_search_insertion_from_item: int64; binary_search_insertion_from_start: int64): IntArray; forward;
function binary_search_insertion(binary_search_insertion_sorted_list: IntArray; binary_search_insertion_item: int64): IntArray; forward;
function merge(merge_left: IntArrayArray; merge_right: IntArrayArray): IntArrayArray; forward;
function sortlist_2d(sortlist_2d_list_2d: IntArrayArray): IntArrayArray; forward;
function merge_insertion_sort(merge_insertion_sort_collection: IntArray): IntArray; forward;
procedure main(); forward;
function binary_search_insertion_from(binary_search_insertion_from_sorted_list: IntArray; binary_search_insertion_from_item: int64; binary_search_insertion_from_start: int64): IntArray;
var
  binary_search_insertion_from_left: int64;
  binary_search_insertion_from_right: integer;
  binary_search_insertion_from_middle: int64;
  binary_search_insertion_from_result_: array of int64;
  binary_search_insertion_from_i: int64;
begin
  binary_search_insertion_from_left := binary_search_insertion_from_start;
  binary_search_insertion_from_right := Length(binary_search_insertion_from_sorted_list) - 1;
  while binary_search_insertion_from_left <= binary_search_insertion_from_right do begin
  binary_search_insertion_from_middle := (binary_search_insertion_from_left + binary_search_insertion_from_right) div 2;
  if binary_search_insertion_from_left = binary_search_insertion_from_right then begin
  if binary_search_insertion_from_sorted_list[binary_search_insertion_from_middle] < binary_search_insertion_from_item then begin
  binary_search_insertion_from_left := binary_search_insertion_from_middle + 1;
end;
  break;
end else begin
  if binary_search_insertion_from_sorted_list[binary_search_insertion_from_middle] < binary_search_insertion_from_item then begin
  binary_search_insertion_from_left := binary_search_insertion_from_middle + 1;
end else begin
  binary_search_insertion_from_right := binary_search_insertion_from_middle - 1;
end;
end;
end;
  binary_search_insertion_from_result_ := [];
  binary_search_insertion_from_i := 0;
  while binary_search_insertion_from_i < binary_search_insertion_from_left do begin
  binary_search_insertion_from_result_ := concat(binary_search_insertion_from_result_, IntArray([binary_search_insertion_from_sorted_list[binary_search_insertion_from_i]]));
  binary_search_insertion_from_i := binary_search_insertion_from_i + 1;
end;
  binary_search_insertion_from_result_ := concat(binary_search_insertion_from_result_, IntArray([binary_search_insertion_from_item]));
  while binary_search_insertion_from_i < Length(binary_search_insertion_from_sorted_list) do begin
  binary_search_insertion_from_result_ := concat(binary_search_insertion_from_result_, IntArray([binary_search_insertion_from_sorted_list[binary_search_insertion_from_i]]));
  binary_search_insertion_from_i := binary_search_insertion_from_i + 1;
end;
  exit(binary_search_insertion_from_result_);
end;
function binary_search_insertion(binary_search_insertion_sorted_list: IntArray; binary_search_insertion_item: int64): IntArray;
begin
  exit(binary_search_insertion_from(binary_search_insertion_sorted_list, binary_search_insertion_item, 0));
end;
function merge(merge_left: IntArrayArray; merge_right: IntArrayArray): IntArrayArray;
var
  merge_result_: array of IntArray;
  merge_i: int64;
  merge_j: int64;
begin
  merge_result_ := [];
  merge_i := 0;
  merge_j := 0;
  while (merge_i < Length(merge_left)) and (merge_j < Length(merge_right)) do begin
  if merge_left[merge_i][0] < merge_right[merge_j][0] then begin
  merge_result_ := concat(merge_result_, [merge_left[merge_i]]);
  merge_i := merge_i + 1;
end else begin
  merge_result_ := concat(merge_result_, [merge_right[merge_j]]);
  merge_j := merge_j + 1;
end;
end;
  while merge_i < Length(merge_left) do begin
  merge_result_ := concat(merge_result_, [merge_left[merge_i]]);
  merge_i := merge_i + 1;
end;
  while merge_j < Length(merge_right) do begin
  merge_result_ := concat(merge_result_, [merge_right[merge_j]]);
  merge_j := merge_j + 1;
end;
  exit(merge_result_);
end;
function sortlist_2d(sortlist_2d_list_2d: IntArrayArray): IntArrayArray;
var
  sortlist_2d_length_: integer;
  sortlist_2d_middle: integer;
  sortlist_2d_left: array of IntArray;
  sortlist_2d_i: int64;
  sortlist_2d_right: array of IntArray;
  sortlist_2d_j: integer;
begin
  sortlist_2d_length_ := Length(sortlist_2d_list_2d);
  if sortlist_2d_length_ <= 1 then begin
  exit(sortlist_2d_list_2d);
end;
  sortlist_2d_middle := sortlist_2d_length_ div 2;
  sortlist_2d_left := [];
  sortlist_2d_i := 0;
  while sortlist_2d_i < sortlist_2d_middle do begin
  sortlist_2d_left := concat(sortlist_2d_left, [sortlist_2d_list_2d[sortlist_2d_i]]);
  sortlist_2d_i := sortlist_2d_i + 1;
end;
  sortlist_2d_right := [];
  sortlist_2d_j := sortlist_2d_middle;
  while sortlist_2d_j < sortlist_2d_length_ do begin
  sortlist_2d_right := concat(sortlist_2d_right, [sortlist_2d_list_2d[sortlist_2d_j]]);
  sortlist_2d_j := sortlist_2d_j + 1;
end;
  exit(merge(sortlist_2d(sortlist_2d_left), sortlist_2d(sortlist_2d_right)));
end;
function merge_insertion_sort(merge_insertion_sort_collection: IntArray): IntArray;
var
  merge_insertion_sort_two_paired_list: array of IntArray;
  merge_insertion_sort_has_last_odd_item: boolean;
  merge_insertion_sort_i: int64;
  merge_insertion_sort_a: int64;
  merge_insertion_sort_b: int64;
  merge_insertion_sort_sorted_list_2d: IntArrayArray;
  merge_insertion_sort_result_: array of int64;
  merge_insertion_sort_inserted_before: boolean;
  merge_insertion_sort_idx: int64;
  merge_insertion_sort_pivot: int64;
begin
  if Length(merge_insertion_sort_collection) <= 1 then begin
  exit(merge_insertion_sort_collection);
end;
  merge_insertion_sort_two_paired_list := [];
  merge_insertion_sort_has_last_odd_item := false;
  merge_insertion_sort_i := 0;
  while merge_insertion_sort_i < Length(merge_insertion_sort_collection) do begin
  if merge_insertion_sort_i = (Length(merge_insertion_sort_collection) - 1) then begin
  merge_insertion_sort_has_last_odd_item := true;
end else begin
  merge_insertion_sort_a := merge_insertion_sort_collection[merge_insertion_sort_i];
  merge_insertion_sort_b := merge_insertion_sort_collection[merge_insertion_sort_i + 1];
  if merge_insertion_sort_a < merge_insertion_sort_b then begin
  merge_insertion_sort_two_paired_list := concat(merge_insertion_sort_two_paired_list, [[merge_insertion_sort_a, merge_insertion_sort_b]]);
end else begin
  merge_insertion_sort_two_paired_list := concat(merge_insertion_sort_two_paired_list, [[merge_insertion_sort_b, merge_insertion_sort_a]]);
end;
end;
  merge_insertion_sort_i := merge_insertion_sort_i + 2;
end;
  merge_insertion_sort_sorted_list_2d := sortlist_2d(merge_insertion_sort_two_paired_list);
  merge_insertion_sort_result_ := [];
  merge_insertion_sort_i := 0;
  while merge_insertion_sort_i < Length(merge_insertion_sort_sorted_list_2d) do begin
  merge_insertion_sort_result_ := concat(merge_insertion_sort_result_, IntArray([merge_insertion_sort_sorted_list_2d[merge_insertion_sort_i][0]]));
  merge_insertion_sort_i := merge_insertion_sort_i + 1;
end;
  merge_insertion_sort_result_ := concat(merge_insertion_sort_result_, IntArray([merge_insertion_sort_sorted_list_2d[Length(merge_insertion_sort_sorted_list_2d) - 1][1]]));
  if merge_insertion_sort_has_last_odd_item then begin
  merge_insertion_sort_result_ := binary_search_insertion(merge_insertion_sort_result_, merge_insertion_sort_collection[Length(merge_insertion_sort_collection) - 1]);
end;
  merge_insertion_sort_inserted_before := false;
  merge_insertion_sort_idx := 0;
  while merge_insertion_sort_idx < (Length(merge_insertion_sort_sorted_list_2d) - 1) do begin
  if merge_insertion_sort_has_last_odd_item and (merge_insertion_sort_result_[merge_insertion_sort_idx] = merge_insertion_sort_collection[Length(merge_insertion_sort_collection) - 1]) then begin
  merge_insertion_sort_inserted_before := true;
end;
  merge_insertion_sort_pivot := merge_insertion_sort_sorted_list_2d[merge_insertion_sort_idx][1];
  if merge_insertion_sort_inserted_before then begin
  merge_insertion_sort_result_ := binary_search_insertion_from(merge_insertion_sort_result_, merge_insertion_sort_pivot, merge_insertion_sort_idx + 2);
end else begin
  merge_insertion_sort_result_ := binary_search_insertion_from(merge_insertion_sort_result_, merge_insertion_sort_pivot, merge_insertion_sort_idx + 1);
end;
  merge_insertion_sort_idx := merge_insertion_sort_idx + 1;
end;
  exit(merge_insertion_sort_result_);
end;
procedure main();
var
  main_example1: array of int64;
  main_example2: array of int64;
  main_example3: array of int64;
begin
  main_example1 := [0, 5, 3, 2, 2];
  main_example2 := [99];
  main_example3 := [-2, -5, -45];
  writeln(list_int_to_str(merge_insertion_sort(main_example1)));
  writeln(list_int_to_str(merge_insertion_sort(main_example2)));
  writeln(list_int_to_str(merge_insertion_sort(main_example3)));
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
