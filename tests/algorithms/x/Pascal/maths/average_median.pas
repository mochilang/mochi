{$mode objfpc}{$modeswitch nestedprocvars}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  nums: IntArray;
function bubble_sort(nums: IntArray): IntArray; forward;
function median(nums: IntArray): real; forward;
function bubble_sort(nums: IntArray): IntArray;
var
  bubble_sort_arr: array of integer;
  bubble_sort_n: integer;
  bubble_sort_i: integer;
  bubble_sort_j: integer;
  bubble_sort_a: integer;
  bubble_sort_b: integer;
begin
  bubble_sort_arr := nums;
  bubble_sort_n := Length(bubble_sort_arr);
  bubble_sort_i := 0;
  while bubble_sort_i < bubble_sort_n do begin
  bubble_sort_j := 0;
  while bubble_sort_j < (bubble_sort_n - 1) do begin
  bubble_sort_a := bubble_sort_arr[bubble_sort_j];
  bubble_sort_b := bubble_sort_arr[bubble_sort_j + 1];
  if bubble_sort_a > bubble_sort_b then begin
  bubble_sort_arr[bubble_sort_j] := bubble_sort_b;
  bubble_sort_arr[bubble_sort_j + 1] := bubble_sort_a;
end;
  bubble_sort_j := bubble_sort_j + 1;
end;
  bubble_sort_i := bubble_sort_i + 1;
end;
  exit(bubble_sort_arr);
end;
function median(nums: IntArray): real;
var
  median_sorted_list: IntArray;
  median_length_: integer;
  median_mid_index: integer;
begin
  median_sorted_list := bubble_sort(nums);
  median_length_ := Length(median_sorted_list);
  median_mid_index := median_length_ div 2;
  if (median_length_ mod 2) = 0 then begin
  exit(Double(median_sorted_list[median_mid_index] + median_sorted_list[median_mid_index - 1]) / 2);
end else begin
  exit(Double(median_sorted_list[median_mid_index]));
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(FloatToStr(median([0])));
  writeln(FloatToStr(median([4, 1, 3, 2])));
  writeln(FloatToStr(median([2, 70, 6, 50, 20, 8, 4])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
