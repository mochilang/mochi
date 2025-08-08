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
  arr1: array of integer;
  arr2: array of integer;
  arr3: array of integer;
  arr4: array of integer;
  arr: IntArray;
function equilibrium_index(arr: IntArray): integer; forward;
function equilibrium_index(arr: IntArray): integer;
var
  equilibrium_index_total: integer;
  equilibrium_index_i: integer;
  equilibrium_index_left: integer;
begin
  equilibrium_index_total := 0;
  equilibrium_index_i := 0;
  while equilibrium_index_i < Length(arr) do begin
  equilibrium_index_total := equilibrium_index_total + arr[equilibrium_index_i];
  equilibrium_index_i := equilibrium_index_i + 1;
end;
  equilibrium_index_left := 0;
  equilibrium_index_i := 0;
  while equilibrium_index_i < Length(arr) do begin
  equilibrium_index_total := equilibrium_index_total - arr[equilibrium_index_i];
  if equilibrium_index_left = equilibrium_index_total then begin
  exit(equilibrium_index_i);
end;
  equilibrium_index_left := equilibrium_index_left + arr[equilibrium_index_i];
  equilibrium_index_i := equilibrium_index_i + 1;
end;
  exit(-1);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  arr1 := [-7, 1, 5, 2, -4, 3, 0];
  writeln(equilibrium_index(arr1));
  arr2 := [1, 2, 3, 4, 5];
  writeln(equilibrium_index(arr2));
  arr3 := [1, 1, 1, 1, 1];
  writeln(equilibrium_index(arr3));
  arr4 := [2, 4, 6, 8, 10, 3];
  writeln(equilibrium_index(arr4));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
