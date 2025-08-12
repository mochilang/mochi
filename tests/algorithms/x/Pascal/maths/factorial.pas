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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  n: integer;
function factorial(n: integer): integer; forward;
function factorial_recursive(n: integer): integer; forward;
procedure test_factorial(); forward;
procedure main(); forward;
function factorial(n: integer): integer;
var
  factorial_value: integer;
  factorial_i: integer;
begin
  if n < 0 then begin
  panic('factorial() not defined for negative values');
end;
  factorial_value := 1;
  factorial_i := 1;
  while factorial_i <= n do begin
  factorial_value := factorial_value * factorial_i;
  factorial_i := factorial_i + 1;
end;
  exit(factorial_value);
end;
function factorial_recursive(n: integer): integer;
begin
  if n < 0 then begin
  panic('factorial() not defined for negative values');
end;
  if n <= 1 then begin
  exit(1);
end;
  exit(n * factorial_recursive(n - 1));
end;
procedure test_factorial();
var
  test_factorial_i: integer;
begin
  test_factorial_i := 0;
  while test_factorial_i <= 10 do begin
  if factorial(test_factorial_i) <> factorial_recursive(test_factorial_i) then begin
  panic('mismatch between factorial and factorial_recursive');
end;
  test_factorial_i := test_factorial_i + 1;
end;
  if factorial(6) <> 720 then begin
  panic('factorial(6) should be 720');
end;
end;
procedure main();
begin
  test_factorial();
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
end.
