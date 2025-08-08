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
  nums: IntArray;
function is_monotonic(nums: IntArray): boolean; forward;
function is_monotonic(nums: IntArray): boolean;
var
  is_monotonic_increasing: boolean;
  is_monotonic_decreasing: boolean;
  is_monotonic_i: integer;
begin
  if Length(nums) <= 2 then begin
  exit(true);
end;
  is_monotonic_increasing := true;
  is_monotonic_decreasing := true;
  is_monotonic_i := 0;
  while is_monotonic_i < (Length(nums) - 1) do begin
  if nums[is_monotonic_i] > nums[is_monotonic_i + 1] then begin
  is_monotonic_increasing := false;
end;
  if nums[is_monotonic_i] < nums[is_monotonic_i + 1] then begin
  is_monotonic_decreasing := false;
end;
  is_monotonic_i := is_monotonic_i + 1;
end;
  exit(is_monotonic_increasing or is_monotonic_decreasing);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(LowerCase(BoolToStr(is_monotonic([1, 2, 2, 3]), true)));
  writeln(LowerCase(BoolToStr(is_monotonic([6, 5, 4, 4]), true)));
  writeln(LowerCase(BoolToStr(is_monotonic([1, 3, 2]), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
