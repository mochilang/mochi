{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
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
  heap: array of real;
  size: int64;
  m: real;
function parent_index(parent_index_child_idx: int64): int64; forward;
function left_child_idx(left_child_idx_parent_idx: int64): int64; forward;
function right_child_idx(right_child_idx_parent_idx: int64): int64; forward;
procedure max_heapify(max_heapify_h: RealArray; max_heapify_heap_size: int64; max_heapify_index: int64); forward;
function build_max_heap(build_max_heap_h: RealArray): int64; forward;
function extract_max(extract_max_h: RealArray; extract_max_heap_size: int64): real; forward;
function insert(insert_h: RealArray; insert_heap_size: int64; insert_value: real): int64; forward;
procedure heap_sort(heap_sort_h: RealArray; heap_sort_heap_size: int64); forward;
function heap_to_string(heap_to_string_h: RealArray; heap_to_string_heap_size: int64): string; forward;
function parent_index(parent_index_child_idx: int64): int64;
begin
  if parent_index_child_idx > 0 then begin
  exit(_floordiv(parent_index_child_idx - 1, 2));
end;
  exit(-1);
end;
function left_child_idx(left_child_idx_parent_idx: int64): int64;
begin
  exit((2 * left_child_idx_parent_idx) + 1);
end;
function right_child_idx(right_child_idx_parent_idx: int64): int64;
begin
  exit((2 * right_child_idx_parent_idx) + 2);
end;
procedure max_heapify(max_heapify_h: RealArray; max_heapify_heap_size: int64; max_heapify_index: int64);
var
  max_heapify_largest: int64;
  max_heapify_left: int64;
  max_heapify_right: int64;
  max_heapify_temp: real;
begin
  max_heapify_largest := max_heapify_index;
  max_heapify_left := left_child_idx(max_heapify_index);
  max_heapify_right := right_child_idx(max_heapify_index);
  if (max_heapify_left < max_heapify_heap_size) and (max_heapify_h[max_heapify_left] > max_heapify_h[max_heapify_largest]) then begin
  max_heapify_largest := max_heapify_left;
end;
  if (max_heapify_right < max_heapify_heap_size) and (max_heapify_h[max_heapify_right] > max_heapify_h[max_heapify_largest]) then begin
  max_heapify_largest := max_heapify_right;
end;
  if max_heapify_largest <> max_heapify_index then begin
  max_heapify_temp := max_heapify_h[max_heapify_index];
  max_heapify_h[max_heapify_index] := max_heapify_h[max_heapify_largest];
  max_heapify_h[max_heapify_largest] := max_heapify_temp;
  max_heapify(max_heapify_h, max_heapify_heap_size, max_heapify_largest);
end;
end;
function build_max_heap(build_max_heap_h: RealArray): int64;
var
  build_max_heap_heap_size: integer;
  build_max_heap_i: integer;
begin
  build_max_heap_heap_size := Length(build_max_heap_h);
  build_max_heap_i := (_floordiv(build_max_heap_heap_size, 2)) - 1;
  while build_max_heap_i >= 0 do begin
  max_heapify(build_max_heap_h, build_max_heap_heap_size, build_max_heap_i);
  build_max_heap_i := build_max_heap_i - 1;
end;
  exit(build_max_heap_heap_size);
end;
function extract_max(extract_max_h: RealArray; extract_max_heap_size: int64): real;
var
  extract_max_max_value: real;
begin
  extract_max_max_value := extract_max_h[0];
  extract_max_h[0] := extract_max_h[extract_max_heap_size - 1];
  max_heapify(extract_max_h, extract_max_heap_size - 1, 0);
  exit(extract_max_max_value);
end;
function insert(insert_h: RealArray; insert_heap_size: int64; insert_value: real): int64;
var
  insert_idx: int64;
begin
  if insert_heap_size < Length(insert_h) then begin
  insert_h[insert_heap_size] := insert_value;
end else begin
  insert_h := concat(insert_h, [insert_value]);
end;
  insert_heap_size := insert_heap_size + 1;
  insert_idx := _floordiv(insert_heap_size - 1, 2);
  while insert_idx >= 0 do begin
  max_heapify(insert_h, insert_heap_size, insert_idx);
  insert_idx := _floordiv(insert_idx - 1, 2);
end;
  exit(insert_heap_size);
end;
procedure heap_sort(heap_sort_h: RealArray; heap_sort_heap_size: int64);
var
  heap_sort_size: int64;
  heap_sort_j: int64;
  heap_sort_temp: real;
begin
  heap_sort_size := heap_sort_heap_size;
  heap_sort_j := heap_sort_size - 1;
  while heap_sort_j > 0 do begin
  heap_sort_temp := heap_sort_h[0];
  heap_sort_h[0] := heap_sort_h[heap_sort_j];
  heap_sort_h[heap_sort_j] := heap_sort_temp;
  heap_sort_size := heap_sort_size - 1;
  max_heapify(heap_sort_h, heap_sort_size, 0);
  heap_sort_j := heap_sort_j - 1;
end;
end;
function heap_to_string(heap_to_string_h: RealArray; heap_to_string_heap_size: int64): string;
var
  heap_to_string_s: string;
  heap_to_string_i: int64;
begin
  heap_to_string_s := '[';
  heap_to_string_i := 0;
  while heap_to_string_i < heap_to_string_heap_size do begin
  heap_to_string_s := heap_to_string_s + FloatToStr(heap_to_string_h[heap_to_string_i]);
  if heap_to_string_i < (heap_to_string_heap_size - 1) then begin
  heap_to_string_s := heap_to_string_s + ', ';
end;
  heap_to_string_i := heap_to_string_i + 1;
end;
  heap_to_string_s := heap_to_string_s + ']';
  exit(heap_to_string_s);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  heap := [103, 9, 1, 7, 11, 15, 25, 201, 209, 107, 5];
  size := build_max_heap(heap);
  writeln(heap_to_string(heap, size));
  m := extract_max(heap, size);
  size := size - 1;
  writeln(FloatToStr(m));
  writeln(heap_to_string(heap, size));
  size := insert(heap, size, 100);
  writeln(heap_to_string(heap, size));
  heap_sort(heap, size);
  writeln(heap_to_string(heap, size));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
