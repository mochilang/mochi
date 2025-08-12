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
  step_size: integer;
  nums: IntArray;
  vector: IntArray;
function unique(nums: IntArray): IntArray; forward;
function array_equalization(vector: IntArray; step_size: integer): integer; forward;
function unique(nums: IntArray): IntArray;
var
  unique_res: array of integer;
  unique_i: integer;
  unique_v: integer;
  unique_found: boolean;
  unique_j: integer;
begin
  unique_res := [];
  unique_i := 0;
  while unique_i < Length(nums) do begin
  unique_v := nums[unique_i];
  unique_found := false;
  unique_j := 0;
  while unique_j < Length(unique_res) do begin
  if unique_res[unique_j] = unique_v then begin
  unique_found := true;
  break;
end;
  unique_j := unique_j + 1;
end;
  if not unique_found then begin
  unique_res := concat(unique_res, IntArray([unique_v]));
end;
  unique_i := unique_i + 1;
end;
  exit(unique_res);
end;
function array_equalization(vector: IntArray; step_size: integer): integer;
var
  array_equalization_elems: IntArray;
  array_equalization_min_updates: integer;
  array_equalization_i: integer;
  array_equalization_target: integer;
  array_equalization_idx: integer;
  array_equalization_updates: integer;
begin
  if step_size <= 0 then begin
  error('Step size must be positive and non-zero.');
end;
  array_equalization_elems := unique(vector);
  array_equalization_min_updates := Length(vector);
  array_equalization_i := 0;
  while array_equalization_i < Length(array_equalization_elems) do begin
  array_equalization_target := array_equalization_elems[array_equalization_i];
  array_equalization_idx := 0;
  array_equalization_updates := 0;
  while array_equalization_idx < Length(vector) do begin
  if vector[array_equalization_idx] <> array_equalization_target then begin
  array_equalization_updates := array_equalization_updates + 1;
  array_equalization_idx := array_equalization_idx + step_size;
end else begin
  array_equalization_idx := array_equalization_idx + 1;
end;
end;
  if array_equalization_updates < array_equalization_min_updates then begin
  array_equalization_min_updates := array_equalization_updates;
end;
  array_equalization_i := array_equalization_i + 1;
end;
  exit(array_equalization_min_updates);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(array_equalization([1, 1, 6, 2, 4, 6, 5, 1, 7, 2, 2, 1, 7, 2, 2], 4)));
  writeln(IntToStr(array_equalization([22, 81, 88, 71, 22, 81, 632, 81, 81, 22, 92], 2)));
  writeln(IntToStr(array_equalization([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], 5)));
  writeln(IntToStr(array_equalization([22, 22, 22, 33, 33, 33], 2)));
  writeln(IntToStr(array_equalization([1, 2, 3], 2147483647)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
