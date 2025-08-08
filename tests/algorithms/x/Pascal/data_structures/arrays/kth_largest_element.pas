{$mode objfpc}
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
  arr1: array of integer;
  arr2: array of integer;
  high: integer;
  position: integer;
  arr: IntArray;
  low: integer;
function partition(arr: IntArray; low: integer; high: integer): integer; forward;
function kth_largest_element(arr: IntArray; position: integer): integer; forward;
function partition(arr: IntArray; low: integer; high: integer): integer;
var
  partition_pivot: integer;
  partition_i: integer;
  partition_j: integer;
  partition_tmp: integer;
  partition_k: integer;
begin
  partition_pivot := arr[high];
  partition_i := low - 1;
  partition_j := low;
  while partition_j < high do begin
  if arr[partition_j] >= partition_pivot then begin
  partition_i := partition_i + 1;
  partition_tmp := arr[partition_i];
  arr[partition_i] := arr[partition_j];
  arr[partition_j] := partition_tmp;
end;
  partition_j := partition_j + 1;
end;
  partition_k := partition_i + 1;
  partition_tmp := arr[partition_k];
  arr[partition_k] := arr[high];
  arr[high] := partition_tmp;
  exit(partition_k);
end;
function kth_largest_element(arr: IntArray; position: integer): integer;
var
  kth_largest_element_low: integer;
  kth_largest_element_high: integer;
  kth_largest_element_pivot_index: integer;
begin
  if Length(arr) = 0 then begin
  exit(-1);
end;
  if (position < 1) or (position > Length(arr)) then begin
  exit(-1);
end;
  kth_largest_element_low := 0;
  kth_largest_element_high := Length(arr) - 1;
  while kth_largest_element_low <= kth_largest_element_high do begin
  if (kth_largest_element_low > (Length(arr) - 1)) or (kth_largest_element_high < 0) then begin
  exit(-1);
end;
  kth_largest_element_pivot_index := partition(arr, kth_largest_element_low, kth_largest_element_high);
  if kth_largest_element_pivot_index = (position - 1) then begin
  exit(arr[kth_largest_element_pivot_index]);
end else begin
  if kth_largest_element_pivot_index > (position - 1) then begin
  kth_largest_element_high := kth_largest_element_pivot_index - 1;
end else begin
  kth_largest_element_low := kth_largest_element_pivot_index + 1;
end;
end;
end;
  exit(-1);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  arr1 := [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5];
  writeln(kth_largest_element(arr1, 3));
  writeln('' + #10 + '');
  arr2 := [2, 5, 6, 1, 9, 3, 8, 4, 7, 3, 5];
  writeln(kth_largest_element(arr2, 1));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
