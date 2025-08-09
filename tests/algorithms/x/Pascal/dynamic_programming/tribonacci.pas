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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  num: integer;
function tribonacci(num: integer): IntArray; forward;
function tribonacci(num: integer): IntArray;
var
  tribonacci_dp: array of integer;
  tribonacci_i: integer;
  tribonacci_t: integer;
begin
  tribonacci_dp := [];
  tribonacci_i := 0;
  while tribonacci_i < num do begin
  if (tribonacci_i = 0) or (tribonacci_i = 1) then begin
  tribonacci_dp := concat(tribonacci_dp, IntArray([0]));
end else begin
  if tribonacci_i = 2 then begin
  tribonacci_dp := concat(tribonacci_dp, IntArray([1]));
end else begin
  tribonacci_t := (tribonacci_dp[tribonacci_i - 1] + tribonacci_dp[tribonacci_i - 2]) + tribonacci_dp[tribonacci_i - 3];
  tribonacci_dp := concat(tribonacci_dp, IntArray([tribonacci_t]));
end;
end;
  tribonacci_i := tribonacci_i + 1;
end;
  exit(tribonacci_dp);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  show_list(tribonacci(8));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
