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
  x: real;
function abs_float(x: real): real; forward;
function average_absolute_deviation(nums: IntArray): real; forward;
function abs_float(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function average_absolute_deviation(nums: IntArray): real;
var
  average_absolute_deviation_sum: integer;
  average_absolute_deviation_x: integer;
  average_absolute_deviation_n: real;
  average_absolute_deviation_mean: real;
  average_absolute_deviation_dev_sum: real;
begin
  if Length(nums) = 0 then begin
  panic('List is empty');
end;
  average_absolute_deviation_sum := 0;
  for average_absolute_deviation_x in nums do begin
  average_absolute_deviation_sum := average_absolute_deviation_sum + average_absolute_deviation_x;
end;
  average_absolute_deviation_n := Double(Length(nums));
  average_absolute_deviation_mean := Double(average_absolute_deviation_sum) / average_absolute_deviation_n;
  average_absolute_deviation_dev_sum := 0;
  for average_absolute_deviation_x in nums do begin
  average_absolute_deviation_dev_sum := average_absolute_deviation_dev_sum + abs_float(Double(average_absolute_deviation_x) - average_absolute_deviation_mean);
end;
  exit(average_absolute_deviation_dev_sum / average_absolute_deviation_n);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(FloatToStr(average_absolute_deviation([0])));
  writeln(FloatToStr(average_absolute_deviation([4, 1, 3, 2])));
  writeln(FloatToStr(average_absolute_deviation([2, 70, 6, 50, 20, 8, 4, 0])));
  writeln(FloatToStr(average_absolute_deviation([-20, 0, 30, 15])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
