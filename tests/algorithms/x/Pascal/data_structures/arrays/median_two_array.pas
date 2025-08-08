{$mode objfpc}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  nums2: RealArray;
  nums1: RealArray;
  xs: RealArray;
function sortFloats(xs: RealArray): RealArray; forward;
function find_median_sorted_arrays(nums1: RealArray; nums2: RealArray): real; forward;
function sortFloats(xs: RealArray): RealArray;
var
  sortFloats_arr: array of real;
  sortFloats_i: integer;
  sortFloats_j: integer;
  sortFloats_t: real;
begin
  sortFloats_arr := xs;
  sortFloats_i := 0;
  while sortFloats_i < Length(sortFloats_arr) do begin
  sortFloats_j := 0;
  while sortFloats_j < (Length(sortFloats_arr) - 1) do begin
  if sortFloats_arr[sortFloats_j] > sortFloats_arr[sortFloats_j + 1] then begin
  sortFloats_t := sortFloats_arr[sortFloats_j];
  sortFloats_arr[sortFloats_j] := sortFloats_arr[sortFloats_j + 1];
  sortFloats_arr[sortFloats_j + 1] := sortFloats_t;
end;
  sortFloats_j := sortFloats_j + 1;
end;
  sortFloats_i := sortFloats_i + 1;
end;
  exit(sortFloats_arr);
end;
function find_median_sorted_arrays(nums1: RealArray; nums2: RealArray): real;
var
  find_median_sorted_arrays_merged: array of real;
  find_median_sorted_arrays_i: integer;
  find_median_sorted_arrays_j: integer;
  find_median_sorted_arrays_sorted: RealArray;
  find_median_sorted_arrays_total: integer;
  find_median_sorted_arrays_middle1: real;
  find_median_sorted_arrays_middle2: real;
begin
  if (Length(nums1) = 0) and (Length(nums2) = 0) then begin
  panic('Both input arrays are empty.');
end;
  find_median_sorted_arrays_merged := [];
  find_median_sorted_arrays_i := 0;
  while find_median_sorted_arrays_i < Length(nums1) do begin
  find_median_sorted_arrays_merged := concat(find_median_sorted_arrays_merged, [nums1[find_median_sorted_arrays_i]]);
  find_median_sorted_arrays_i := find_median_sorted_arrays_i + 1;
end;
  find_median_sorted_arrays_j := 0;
  while find_median_sorted_arrays_j < Length(nums2) do begin
  find_median_sorted_arrays_merged := concat(find_median_sorted_arrays_merged, [nums2[find_median_sorted_arrays_j]]);
  find_median_sorted_arrays_j := find_median_sorted_arrays_j + 1;
end;
  find_median_sorted_arrays_sorted := sortFloats(find_median_sorted_arrays_merged);
  find_median_sorted_arrays_total := Length(find_median_sorted_arrays_sorted);
  if (find_median_sorted_arrays_total mod 2) = 1 then begin
  exit(find_median_sorted_arrays_sorted[find_median_sorted_arrays_total div 2]);
end;
  find_median_sorted_arrays_middle1 := find_median_sorted_arrays_sorted[(find_median_sorted_arrays_total div 2) - 1];
  find_median_sorted_arrays_middle2 := find_median_sorted_arrays_sorted[find_median_sorted_arrays_total div 2];
  exit((find_median_sorted_arrays_middle1 + find_median_sorted_arrays_middle2) / 2);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(find_median_sorted_arrays([1, 3], [2]));
  writeln(find_median_sorted_arrays([1, 2], [3, 4]));
  writeln(find_median_sorted_arrays([0, 0], [0, 0]));
  writeln(find_median_sorted_arrays(RealArray([]), [1]));
  writeln(find_median_sorted_arrays([-1000], [1000]));
  writeln(find_median_sorted_arrays([-1.1, -2.2], [-3.3, -4.4]));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
