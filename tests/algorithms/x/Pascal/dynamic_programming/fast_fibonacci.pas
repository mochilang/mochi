{$mode objfpc}
program Main;
uses SysUtils;
type FibPair = record
  fn: integer;
  fn1: integer;
end;
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
  i: integer;
  n: integer;
function makeFibPair(fn: integer; fn1: integer): FibPair; forward;
function _fib(n: integer): FibPair; forward;
function fibonacci(n: integer): integer; forward;
function makeFibPair(fn: integer; fn1: integer): FibPair;
begin
  Result.fn := fn;
  Result.fn1 := fn1;
end;
function _fib(n: integer): FibPair;
var
  _fib_half: FibPair;
  _fib_a: integer;
  _fib_b: integer;
  _fib_c: integer;
  _fib_d: integer;
begin
  if n = 0 then begin
  exit(makeFibPair(0, 1));
end;
  _fib_half := _fib(n div 2);
  _fib_a := _fib_half.fn;
  _fib_b := _fib_half.fn1;
  _fib_c := _fib_a * ((_fib_b * 2) - _fib_a);
  _fib_d := (_fib_a * _fib_a) + (_fib_b * _fib_b);
  if (n mod 2) = 0 then begin
  exit(makeFibPair(_fib_c, _fib_d));
end;
  exit(makeFibPair(_fib_d, _fib_c + _fib_d));
end;
function fibonacci(n: integer): integer;
var
  fibonacci_res: FibPair;
begin
  if n < 0 then begin
  panic('Negative arguments are not supported');
end;
  fibonacci_res := _fib(n);
  exit(fibonacci_res.fn);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  i := 0;
  while i < 13 do begin
  writeln(IntToStr(fibonacci(i)));
  i := i + 1;
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
