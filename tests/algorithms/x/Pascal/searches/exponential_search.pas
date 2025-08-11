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
  xs: IntArray;
  arr: IntArray;
  item: integer;
function is_sorted(xs: IntArray): boolean; forward;
function exponential_search(arr: IntArray; item: integer): integer; forward;
function is_sorted(xs: IntArray): boolean;
var
  is_sorted_i: integer;
begin
  is_sorted_i := 1;
  while is_sorted_i < Length(xs) do begin
  if xs[is_sorted_i - 1] > xs[is_sorted_i] then begin
  exit(false);
end;
  is_sorted_i := is_sorted_i + 1;
end;
  exit(true);
end;
function exponential_search(arr: IntArray; item: integer): integer;
var
  exponential_search_bound: integer;
  exponential_search_left: integer;
  exponential_search_right: integer;
  exponential_search_mid: integer;
begin
  if not is_sorted(arr) then begin
  panic('sorted_collection must be sorted in ascending order');
end;
  if Length(arr) = 0 then begin
  exit(-1);
end;
  if arr[0] = item then begin
  exit(0);
end;
  exponential_search_bound := 1;
  while (exponential_search_bound < Length(arr)) and (arr[exponential_search_bound] < item) do begin
  exponential_search_bound := exponential_search_bound * 2;
end;
  exponential_search_left := exponential_search_bound div 2;
  exponential_search_right := exponential_search_bound;
  if exponential_search_right >= Length(arr) then begin
  exponential_search_right := Length(arr) - 1;
end;
  while exponential_search_left <= exponential_search_right do begin
  exponential_search_mid := exponential_search_left + ((exponential_search_right - exponential_search_left) div 2);
  if arr[exponential_search_mid] = item then begin
  exit(exponential_search_mid);
end;
  if arr[exponential_search_mid] > item then begin
  exponential_search_right := exponential_search_mid - 1;
end else begin
  exponential_search_left := exponential_search_mid + 1;
end;
end;
  exit(-1);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
