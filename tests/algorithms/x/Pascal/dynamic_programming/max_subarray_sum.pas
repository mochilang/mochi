{$mode objfpc}{$modeswitch nestedprocvars}
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
  empty: array of real;
function max_subarray_sum(max_subarray_sum_nums: RealArray; max_subarray_sum_allow_empty: boolean): real; forward;
function max_subarray_sum(max_subarray_sum_nums: RealArray; max_subarray_sum_allow_empty: boolean): real;
var
  max_subarray_sum_max_sum: real;
  max_subarray_sum_curr_sum: real;
  max_subarray_sum_i: int64;
  max_subarray_sum_num: real;
  max_subarray_sum_temp: real;
  max_subarray_sum_i_9: int64;
  max_subarray_sum_num_10: real;
  max_subarray_sum_temp_11: real;
begin
  if Length(max_subarray_sum_nums) = 0 then begin
  exit(0);
end;
  max_subarray_sum_max_sum := 0;
  max_subarray_sum_curr_sum := 0;
  if max_subarray_sum_allow_empty then begin
  max_subarray_sum_max_sum := 0;
  max_subarray_sum_curr_sum := 0;
  max_subarray_sum_i := 0;
  while max_subarray_sum_i < Length(max_subarray_sum_nums) do begin
  max_subarray_sum_num := max_subarray_sum_nums[max_subarray_sum_i];
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
  max_subarray_sum_max_sum := max_subarray_sum_nums[0];
  max_subarray_sum_curr_sum := max_subarray_sum_nums[0];
  max_subarray_sum_i_9 := 1;
  while max_subarray_sum_i_9 < Length(max_subarray_sum_nums) do begin
  max_subarray_sum_num_10 := max_subarray_sum_nums[max_subarray_sum_i_9];
  max_subarray_sum_temp_11 := max_subarray_sum_curr_sum + max_subarray_sum_num_10;
  if max_subarray_sum_temp_11 > max_subarray_sum_num_10 then begin
  max_subarray_sum_curr_sum := max_subarray_sum_temp_11;
end else begin
  max_subarray_sum_curr_sum := max_subarray_sum_num_10;
end;
  if max_subarray_sum_curr_sum > max_subarray_sum_max_sum then begin
  max_subarray_sum_max_sum := max_subarray_sum_curr_sum;
end;
  max_subarray_sum_i_9 := max_subarray_sum_i_9 + 1;
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
  writeln('');
end.
