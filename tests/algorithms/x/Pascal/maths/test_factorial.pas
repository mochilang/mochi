{$mode objfpc}{$modeswitch nestedprocvars}
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function factorial(factorial_n: int64): int64; forward;
function factorial_recursive(factorial_recursive_n: int64): int64; forward;
procedure test_zero(); forward;
procedure test_positive_integers(); forward;
procedure test_large_number(); forward;
procedure run_tests(); forward;
procedure main(); forward;
function factorial(factorial_n: int64): int64;
var
  factorial_value: int64;
  factorial_i: int64;
begin
  if factorial_n < 0 then begin
  panic('factorial() not defined for negative values');
end;
  factorial_value := 1;
  factorial_i := 1;
  while factorial_i <= factorial_n do begin
  factorial_value := factorial_value * factorial_i;
  factorial_i := factorial_i + 1;
end;
  exit(factorial_value);
end;
function factorial_recursive(factorial_recursive_n: int64): int64;
begin
  if factorial_recursive_n < 0 then begin
  panic('factorial() not defined for negative values');
end;
  if factorial_recursive_n <= 1 then begin
  exit(1);
end;
  exit(factorial_recursive_n * factorial_recursive(factorial_recursive_n - 1));
end;
procedure test_zero();
begin
  if factorial(0) <> 1 then begin
  panic('factorial(0) failed');
end;
  if factorial_recursive(0) <> 1 then begin
  panic('factorial_recursive(0) failed');
end;
end;
procedure test_positive_integers();
begin
  if factorial(1) <> 1 then begin
  panic('factorial(1) failed');
end;
  if factorial_recursive(1) <> 1 then begin
  panic('factorial_recursive(1) failed');
end;
  if factorial(5) <> 120 then begin
  panic('factorial(5) failed');
end;
  if factorial_recursive(5) <> 120 then begin
  panic('factorial_recursive(5) failed');
end;
  if factorial(7) <> 5040 then begin
  panic('factorial(7) failed');
end;
  if factorial_recursive(7) <> 5040 then begin
  panic('factorial_recursive(7) failed');
end;
end;
procedure test_large_number();
begin
  if factorial(10) <> 3628800 then begin
  panic('factorial(10) failed');
end;
  if factorial_recursive(10) <> 3628800 then begin
  panic('factorial_recursive(10) failed');
end;
end;
procedure run_tests();
begin
  test_zero();
  test_positive_integers();
  test_large_number();
end;
procedure main();
begin
  run_tests();
  writeln(factorial(6));
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
  writeln('');
end.
