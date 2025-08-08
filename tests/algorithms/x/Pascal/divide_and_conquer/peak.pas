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
  lst: IntArray;
function peak(lst: IntArray): integer; forward;
procedure main(); forward;
function peak(lst: IntArray): integer;
var
  peak_low: integer;
  peak_high: integer;
  peak_mid: integer;
begin
  peak_low := 0;
  peak_high := Length(lst) - 1;
  while peak_low < peak_high do begin
  peak_mid := (peak_low + peak_high) div 2;
  if lst[peak_mid] < lst[peak_mid + 1] then begin
  peak_low := peak_mid + 1;
end else begin
  peak_high := peak_mid;
end;
end;
  exit(lst[peak_low]);
end;
procedure main();
begin
  writeln(IntToStr(peak([1, 2, 3, 4, 5, 4, 3, 2, 1])));
  writeln(IntToStr(peak([1, 10, 9, 8, 7, 6, 5, 4])));
  writeln(IntToStr(peak([1, 9, 8, 7])));
  writeln(IntToStr(peak([1, 2, 3, 4, 5, 6, 7, 0])));
  writeln(IntToStr(peak([1, 2, 3, 4, 3, 2, 1, 0, -1, -2])));
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
