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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  arr: IntArray;
  k: integer;
function max_sum_sliding_window(arr: IntArray; k: integer): integer; forward;
procedure test_max_sum_sliding_window(); forward;
procedure main(); forward;
function max_sum_sliding_window(arr: IntArray; k: integer): integer;
var
  max_sum_sliding_window_idx: integer;
  max_sum_sliding_window_current_sum: integer;
  max_sum_sliding_window_max_sum: integer;
  max_sum_sliding_window_i: integer;
begin
  if (k < 0) or (Length(arr) < k) then begin
  panic('Invalid Input');
end;
  max_sum_sliding_window_idx := 0;
  max_sum_sliding_window_current_sum := 0;
  while max_sum_sliding_window_idx < k do begin
  max_sum_sliding_window_current_sum := max_sum_sliding_window_current_sum + arr[max_sum_sliding_window_idx];
  max_sum_sliding_window_idx := max_sum_sliding_window_idx + 1;
end;
  max_sum_sliding_window_max_sum := max_sum_sliding_window_current_sum;
  max_sum_sliding_window_i := 0;
  while max_sum_sliding_window_i < (Length(arr) - k) do begin
  max_sum_sliding_window_current_sum := (max_sum_sliding_window_current_sum - arr[max_sum_sliding_window_i]) + arr[max_sum_sliding_window_i + k];
  if max_sum_sliding_window_current_sum > max_sum_sliding_window_max_sum then begin
  max_sum_sliding_window_max_sum := max_sum_sliding_window_current_sum;
end;
  max_sum_sliding_window_i := max_sum_sliding_window_i + 1;
end;
  exit(max_sum_sliding_window_max_sum);
end;
procedure test_max_sum_sliding_window();
var
  test_max_sum_sliding_window_arr1: array of integer;
  test_max_sum_sliding_window_arr2: array of integer;
begin
  test_max_sum_sliding_window_arr1 := [1, 4, 2, 10, 2, 3, 1, 0, 20];
  if max_sum_sliding_window(test_max_sum_sliding_window_arr1, 4) <> 24 then begin
  panic('test1 failed');
end;
  test_max_sum_sliding_window_arr2 := [1, 4, 2, 10, 2, 13, 1, 0, 2];
  if max_sum_sliding_window(test_max_sum_sliding_window_arr2, 4) <> 27 then begin
  panic('test2 failed');
end;
end;
procedure main();
var
  main_sample: array of integer;
begin
  test_max_sum_sliding_window();
  main_sample := [1, 4, 2, 10, 2, 3, 1, 0, 20];
  writeln(IntToStr(max_sum_sliding_window(main_sample, 4)));
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
end.
