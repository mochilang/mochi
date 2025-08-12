{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
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
  x: real;
function floor(x: real): integer; forward;
procedure test_floor(); forward;
procedure main(); forward;
function floor(x: real): integer;
var
  floor_i: integer;
begin
  floor_i := Trunc(x);
  if (x - Double(floor_i)) >= 0 then begin
  exit(floor_i);
end;
  exit(floor_i - 1);
end;
procedure test_floor();
var
  test_floor_nums: array of real;
  test_floor_expected: array of integer;
  test_floor_idx: integer;
begin
  test_floor_nums := [1, -1, 0, 0, 1.1, -1.1, 1, -1, 1e+09];
  test_floor_expected := [1, -1, 0, 0, 1, -2, 1, -1, 1000000000];
  test_floor_idx := 0;
  while test_floor_idx < Length(test_floor_nums) do begin
  if Floor(test_floor_nums[test_floor_idx]) <> test_floor_expected[test_floor_idx] then begin
  panic('floor test failed');
end;
  test_floor_idx := test_floor_idx + 1;
end;
end;
procedure main();
begin
  test_floor();
  writeln(IntToStr(Floor(-1.1)));
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
