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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  x: integer;
function bsearch(arr: IntArray; x: integer): integer; forward;
function bsearchRec(arr: IntArray; x: integer; low: integer; high: integer): integer; forward;
procedure main(); forward;
function bsearch(arr: IntArray; x: integer): integer;
var
  bsearch_low: integer;
  bsearch_high: integer;
  bsearch_mid: integer;
begin
  bsearch_low := 0;
  bsearch_high := Length(arr) - 1;
  while bsearch_low <= bsearch_high do begin
  bsearch_mid := (bsearch_low + bsearch_high) div 2;
  if arr[bsearch_mid] > x then begin
  bsearch_high := bsearch_mid - 1;
end else begin
  if arr[bsearch_mid] < x then begin
  bsearch_low := bsearch_mid + 1;
end else begin
  exit(bsearch_mid);
end;
end;
end;
  exit(-1);
end;
function bsearchRec(arr: IntArray; x: integer; low: integer; high: integer): integer;
var
  bsearchRec_mid: integer;
begin
  if high < low then begin
  exit(-1);
end;
  bsearchRec_mid := (low + high) div 2;
  if arr[bsearchRec_mid] > x then begin
  exit(bsearchRec(arr, x, low, bsearchRec_mid - 1));
end else begin
  if arr[bsearchRec_mid] < x then begin
  exit(bsearchRec(arr, x, bsearchRec_mid + 1, high));
end;
end;
  exit(bsearchRec_mid);
end;
procedure main();
var
  main_nums: array of integer;
  main_idx: integer;
begin
  main_nums := [-31, 0, 1, 2, 2, 4, 65, 83, 99, 782];
  x := 2;
  main_idx := bsearch(main_nums, x);
  if main_idx >= 0 then begin
  writeln(((IntToStr(x) + ' is at index ') + IntToStr(main_idx)) + '.');
end else begin
  writeln(IntToStr(x) + ' is not found.');
end;
  x := 5;
  main_idx := bsearchRec(main_nums, x, 0, Length(main_nums) - 1);
  if main_idx >= 0 then begin
  writeln(((IntToStr(x) + ' is at index ') + IntToStr(main_idx)) + '.');
end else begin
  writeln(IntToStr(x) + ' is not found.');
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
