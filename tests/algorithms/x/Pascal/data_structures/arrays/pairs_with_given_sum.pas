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
  req_sum: integer;
  arr: IntArray;
function pairs_with_sum(arr: IntArray; req_sum: integer): integer; forward;
function pairs_with_sum(arr: IntArray; req_sum: integer): integer;
var
  pairs_with_sum_n: integer;
  pairs_with_sum_count: integer;
  pairs_with_sum_i: integer;
  pairs_with_sum_j: integer;
begin
  pairs_with_sum_n := Length(arr);
  pairs_with_sum_count := 0;
  pairs_with_sum_i := 0;
  while pairs_with_sum_i < pairs_with_sum_n do begin
  pairs_with_sum_j := pairs_with_sum_i + 1;
  while pairs_with_sum_j < pairs_with_sum_n do begin
  if (arr[pairs_with_sum_i] + arr[pairs_with_sum_j]) = req_sum then begin
  pairs_with_sum_count := pairs_with_sum_count + 1;
end;
  pairs_with_sum_j := pairs_with_sum_j + 1;
end;
  pairs_with_sum_i := pairs_with_sum_i + 1;
end;
  exit(pairs_with_sum_count);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(pairs_with_sum([1, 5, 7, 1], 6));
  writeln(pairs_with_sum([1, 1, 1, 1, 1, 1, 1, 1], 2));
  writeln(pairs_with_sum([1, 7, 6, 2, 5, 4, 3, 1, 9, 8], 7));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
