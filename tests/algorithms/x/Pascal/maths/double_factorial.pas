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
  n: integer;
function double_factorial_recursive(n: integer): integer; forward;
function double_factorial_iterative(n: integer): integer; forward;
procedure test_double_factorial(); forward;
procedure main(); forward;
function double_factorial_recursive(n: integer): integer;
begin
  if n < 0 then begin
  panic('double_factorial_recursive() not defined for negative values');
end;
  if n <= 1 then begin
  exit(1);
end;
  exit(n * double_factorial_recursive(n - 2));
end;
function double_factorial_iterative(n: integer): integer;
var
  double_factorial_iterative_result_: integer;
  double_factorial_iterative_i: integer;
begin
  if n < 0 then begin
  panic('double_factorial_iterative() not defined for negative values');
end;
  double_factorial_iterative_result_ := 1;
  double_factorial_iterative_i := n;
  while double_factorial_iterative_i > 0 do begin
  double_factorial_iterative_result_ := double_factorial_iterative_result_ * double_factorial_iterative_i;
  double_factorial_iterative_i := double_factorial_iterative_i - 2;
end;
  exit(double_factorial_iterative_result_);
end;
procedure test_double_factorial();
var
  test_double_factorial_n: integer;
begin
  if double_factorial_recursive(0) <> 1 then begin
  panic('0!! recursive failed');
end;
  if double_factorial_iterative(0) <> 1 then begin
  panic('0!! iterative failed');
end;
  if double_factorial_recursive(1) <> 1 then begin
  panic('1!! recursive failed');
end;
  if double_factorial_iterative(1) <> 1 then begin
  panic('1!! iterative failed');
end;
  if double_factorial_recursive(5) <> 15 then begin
  panic('5!! recursive failed');
end;
  if double_factorial_iterative(5) <> 15 then begin
  panic('5!! iterative failed');
end;
  if double_factorial_recursive(6) <> 48 then begin
  panic('6!! recursive failed');
end;
  if double_factorial_iterative(6) <> 48 then begin
  panic('6!! iterative failed');
end;
  test_double_factorial_n := 0;
  while test_double_factorial_n <= 10 do begin
  if double_factorial_recursive(test_double_factorial_n) <> double_factorial_iterative(test_double_factorial_n) then begin
  panic('double factorial mismatch');
end;
  test_double_factorial_n := test_double_factorial_n + 1;
end;
end;
procedure main();
begin
  test_double_factorial();
  writeln(double_factorial_iterative(10));
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
