{$mode objfpc}
program Main;
uses SysUtils, Math;
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
  empty: array of real;
  allow_empty: boolean;
  nums: RealArray;
function max_subarray_sum(nums: RealArray; allow_empty: boolean): real; forward;
function max_subarray_sum(nums: RealArray; allow_empty: boolean): real;
var
  max_subarray_sum_max_sum: real;
  max_subarray_sum_curr_sum: real;
  max_subarray_sum_i: integer;
  max_subarray_sum_num: real;
  max_subarray_sum_temp: real;
begin
  if Length(nums) = 0 then begin
  exit(0);
end;
  max_subarray_sum_max_sum := 0;
  max_subarray_sum_curr_sum := 0;
  if allow_empty then begin
  max_subarray_sum_max_sum := 0;
  max_subarray_sum_curr_sum := 0;
  max_subarray_sum_i := 0;
  while max_subarray_sum_i < Length(nums) do begin
  max_subarray_sum_num := nums[max_subarray_sum_i];
  max_subarray_sum_temp := max_subarray_sum_curr_sum + max_subarray_sum_num;
  if max_subarray_sum_temp > 0 then begin
  max_subarray_sum_curr_sum := max_subarray_sum_temp;
end else begin
  max_subarray_sum_curr_sum := 0;
end;
  if max_subarray_sum_curr_sum > max_subarray_sum_max_sum then begin
  max_subarray_sum_max_sum := max_subarray_sum_curr_sum;
end;
  max_subarray_sum_i := max_subarray_sum_i + 1;
end;
end else begin
  max_subarray_sum_max_sum := nums[0];
  max_subarray_sum_curr_sum := nums[0];
  max_subarray_sum_i := 1;
  while max_subarray_sum_i < Length(nums) do begin
  max_subarray_sum_num := nums[max_subarray_sum_i];
  max_subarray_sum_temp := max_subarray_sum_curr_sum + max_subarray_sum_num;
  if max_subarray_sum_temp > max_subarray_sum_num then begin
  max_subarray_sum_curr_sum := max_subarray_sum_temp;
end else begin
  max_subarray_sum_curr_sum := max_subarray_sum_num;
end;
  if max_subarray_sum_curr_sum > max_subarray_sum_max_sum then begin
  max_subarray_sum_max_sum := max_subarray_sum_curr_sum;
end;
  max_subarray_sum_i := max_subarray_sum_i + 1;
end;
end;
  exit(max_subarray_sum_max_sum);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(FloatToStr(max_subarray_sum([2, 8, 9], false)));
  writeln(FloatToStr(max_subarray_sum([0, 0], false)));
  writeln(FloatToStr(max_subarray_sum([-1, 0, 1], false)));
  writeln(FloatToStr(max_subarray_sum([1, 2, 3, 4, -2], false)));
  writeln(FloatToStr(max_subarray_sum([-2, 1, -3, 4, -1, 2, 1, -5, 4], false)));
  writeln(FloatToStr(max_subarray_sum([2, 3, -9, 8, -2], false)));
  writeln(FloatToStr(max_subarray_sum([-2, -3, -1, -4, -6], false)));
  writeln(FloatToStr(max_subarray_sum([-2, -3, -1, -4, -6], true)));
  empty := [];
  writeln(FloatToStr(max_subarray_sum(empty, false)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
