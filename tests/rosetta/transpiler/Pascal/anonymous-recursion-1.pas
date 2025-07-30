{$mode objfpc}
program Main;
uses SysUtils;
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
function fib(n: integer): integer; forward;
procedure main(); forward;
function fib(n: integer): integer;
var
  fib_a: integer;
  fib_b: integer;
  fib_i: integer;
  fib_t: integer;
begin
  if n < 2 then begin
  exit(n);
end;
  fib_a := 0;
  fib_b := 1;
  fib_i := 1;
  while fib_i < n do begin
  fib_t := fib_a + fib_b;
  fib_a := fib_b;
  fib_b := fib_t;
  fib_i := fib_i + 1;
end;
  exit(fib_b);
end;
procedure main();
var
  main_n: integer;
begin
  for main_n in [0, 1, 2, 3, 4, 5, 10, 40, -1] do begin
  if main_n < 0 then begin
  writeln('fib undefined for negative numbers');
end else begin
  writeln((('fib ' + IntToStr(main_n)) + ' = ') + IntToStr(fib(main_n)));
end;
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
