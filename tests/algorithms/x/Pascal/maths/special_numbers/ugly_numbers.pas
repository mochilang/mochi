{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type IntArray = array of int64;
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
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function ugly_numbers(ugly_numbers_n: int64): int64; forward;
function ugly_numbers(ugly_numbers_n: int64): int64;
var
  ugly_numbers_ugly_nums: array of int64;
  ugly_numbers_i2: int64;
  ugly_numbers_i3: int64;
  ugly_numbers_i5: int64;
  ugly_numbers_next_2: int64;
  ugly_numbers_next_3: int64;
  ugly_numbers_next_5: int64;
  ugly_numbers_count: int64;
  ugly_numbers_next_num: int64;
begin
  if ugly_numbers_n <= 0 then begin
  exit(1);
end;
  ugly_numbers_ugly_nums := [];
  ugly_numbers_ugly_nums := concat(ugly_numbers_ugly_nums, IntArray([1]));
  ugly_numbers_i2 := 0;
  ugly_numbers_i3 := 0;
  ugly_numbers_i5 := 0;
  ugly_numbers_next_2 := 2;
  ugly_numbers_next_3 := 3;
  ugly_numbers_next_5 := 5;
  ugly_numbers_count := 1;
  while ugly_numbers_count < ugly_numbers_n do begin
  if ugly_numbers_next_2 < ugly_numbers_next_3 then begin
  ugly_numbers_next_num := IfThen(ugly_numbers_next_2 < ugly_numbers_next_5, ugly_numbers_next_2, ugly_numbers_next_5);
end else begin
  ugly_numbers_next_num := IfThen(ugly_numbers_next_3 < ugly_numbers_next_5, ugly_numbers_next_3, ugly_numbers_next_5);
end;
  ugly_numbers_ugly_nums := concat(ugly_numbers_ugly_nums, IntArray([ugly_numbers_next_num]));
  if ugly_numbers_next_num = ugly_numbers_next_2 then begin
  ugly_numbers_i2 := ugly_numbers_i2 + 1;
  ugly_numbers_next_2 := ugly_numbers_ugly_nums[ugly_numbers_i2] * 2;
end;
  if ugly_numbers_next_num = ugly_numbers_next_3 then begin
  ugly_numbers_i3 := ugly_numbers_i3 + 1;
  ugly_numbers_next_3 := ugly_numbers_ugly_nums[ugly_numbers_i3] * 3;
end;
  if ugly_numbers_next_num = ugly_numbers_next_5 then begin
  ugly_numbers_i5 := ugly_numbers_i5 + 1;
  ugly_numbers_next_5 := ugly_numbers_ugly_nums[ugly_numbers_i5] * 5;
end;
  ugly_numbers_count := ugly_numbers_count + 1;
end;
  exit(ugly_numbers_ugly_nums[Length(ugly_numbers_ugly_nums) - 1]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(ugly_numbers(100)));
  writeln(IntToStr(ugly_numbers(0)));
  writeln(IntToStr(ugly_numbers(20)));
  writeln(IntToStr(ugly_numbers(-5)));
  writeln(IntToStr(ugly_numbers(200)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
