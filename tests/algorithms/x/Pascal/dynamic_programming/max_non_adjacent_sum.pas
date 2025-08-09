{$mode objfpc}
program Main;
uses SysUtils, Math;
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
  nums: IntArray;
function maximum_non_adjacent_sum(nums: IntArray): integer; forward;
function maximum_non_adjacent_sum(nums: IntArray): integer;
var
  maximum_non_adjacent_sum_max_including: integer;
  maximum_non_adjacent_sum_max_excluding: integer;
  maximum_non_adjacent_sum_i: integer;
  maximum_non_adjacent_sum_num: integer;
  maximum_non_adjacent_sum_new_including: integer;
  maximum_non_adjacent_sum_new_excluding: integer;
begin
  if Length(nums) = 0 then begin
  exit(0);
end;
  maximum_non_adjacent_sum_max_including := nums[0];
  maximum_non_adjacent_sum_max_excluding := 0;
  maximum_non_adjacent_sum_i := 1;
  while maximum_non_adjacent_sum_i < Length(nums) do begin
  maximum_non_adjacent_sum_num := nums[maximum_non_adjacent_sum_i];
  maximum_non_adjacent_sum_new_including := maximum_non_adjacent_sum_max_excluding + maximum_non_adjacent_sum_num;
  if maximum_non_adjacent_sum_max_including > maximum_non_adjacent_sum_max_excluding then begin
  maximum_non_adjacent_sum_new_excluding := maximum_non_adjacent_sum_max_including;
end else begin
  maximum_non_adjacent_sum_new_excluding := maximum_non_adjacent_sum_max_excluding;
end;
  maximum_non_adjacent_sum_max_including := maximum_non_adjacent_sum_new_including;
  maximum_non_adjacent_sum_max_excluding := maximum_non_adjacent_sum_new_excluding;
  maximum_non_adjacent_sum_i := maximum_non_adjacent_sum_i + 1;
end;
  if maximum_non_adjacent_sum_max_including > maximum_non_adjacent_sum_max_excluding then begin
  exit(maximum_non_adjacent_sum_max_including);
end;
  exit(maximum_non_adjacent_sum_max_excluding);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(maximum_non_adjacent_sum([1, 2, 3])));
  writeln(IntToStr(maximum_non_adjacent_sum([1, 5, 3, 7, 2, 2, 6])));
  writeln(IntToStr(maximum_non_adjacent_sum([-1, -5, -3, -7, -2, -2, -6])));
  writeln(IntToStr(maximum_non_adjacent_sum([499, 500, -3, -7, -2, -2, -6])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
