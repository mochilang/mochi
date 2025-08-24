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
procedure show_list_int64(xs: array of int64);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
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
  data: array of int64;
  result_: IntArray;
procedure heapify(heapify_arr: IntArray; heapify_index: int64; heapify_heap_size: int64); forward;
function heap_sort(heap_sort_arr: IntArray): IntArray; forward;
procedure heapify(heapify_arr: IntArray; heapify_index: int64; heapify_heap_size: int64);
var
  heapify_largest: int64;
  heapify_left_index: int64;
  heapify_right_index: int64;
  heapify_temp: int64;
begin
  heapify_largest := heapify_index;
  heapify_left_index := (2 * heapify_index) + 1;
  heapify_right_index := (2 * heapify_index) + 2;
  if (heapify_left_index < heapify_heap_size) and (heapify_arr[heapify_left_index] > heapify_arr[heapify_largest]) then begin
  heapify_largest := heapify_left_index;
end;
  if (heapify_right_index < heapify_heap_size) and (heapify_arr[heapify_right_index] > heapify_arr[heapify_largest]) then begin
  heapify_largest := heapify_right_index;
end;
  if heapify_largest <> heapify_index then begin
  heapify_temp := heapify_arr[heapify_largest];
  heapify_arr[heapify_largest] := heapify_arr[heapify_index];
  heapify_arr[heapify_index] := heapify_temp;
  heapify(heapify_arr, heapify_largest, heapify_heap_size);
end;
end;
function heap_sort(heap_sort_arr: IntArray): IntArray;
var
  heap_sort_n: integer;
  heap_sort_i: integer;
  heap_sort_temp: int64;
begin
  heap_sort_n := Length(heap_sort_arr);
  heap_sort_i := (heap_sort_n div 2) - 1;
  while heap_sort_i >= 0 do begin
  heapify(heap_sort_arr, heap_sort_i, heap_sort_n);
  heap_sort_i := heap_sort_i - 1;
end;
  heap_sort_i := heap_sort_n - 1;
  while heap_sort_i > 0 do begin
  heap_sort_temp := heap_sort_arr[0];
  heap_sort_arr[0] := heap_sort_arr[heap_sort_i];
  heap_sort_arr[heap_sort_i] := heap_sort_temp;
  heapify(heap_sort_arr, 0, heap_sort_i);
  heap_sort_i := heap_sort_i - 1;
end;
  exit(heap_sort_arr);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  data := [3, 7, 9, 28, 123, -5, 8, -30, -200, 0, 4];
  result_ := heap_sort(data);
  show_list_int64(result_);
  if list_int_to_str(result_) <> list_int_to_str([-200, -30, -5, 0, 3, 4, 7, 8, 9, 28, 123]) then begin
  panic('Assertion error');
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
