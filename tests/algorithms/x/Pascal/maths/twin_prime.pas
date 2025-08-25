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
function is_prime(is_prime_n: int64): boolean; forward;
function twin_prime(twin_prime_number: int64): int64; forward;
procedure test_twin_prime(); forward;
procedure main(); forward;
function is_prime(is_prime_n: int64): boolean;
var
  is_prime_i: int64;
begin
  if is_prime_n < 2 then begin
  exit(false);
end;
  if (is_prime_n mod 2) = 0 then begin
  exit(is_prime_n = 2);
end;
  is_prime_i := 3;
  while (is_prime_i * is_prime_i) <= is_prime_n do begin
  if (is_prime_n mod is_prime_i) = 0 then begin
  exit(false);
end;
  is_prime_i := is_prime_i + 2;
end;
  exit(true);
end;
function twin_prime(twin_prime_number: int64): int64;
begin
  if is_prime(twin_prime_number) and is_prime(twin_prime_number + 2) then begin
  exit(twin_prime_number + 2);
end;
  exit(-1);
end;
procedure test_twin_prime();
begin
  if twin_prime(3) <> 5 then begin
  panic('twin_prime(3) failed');
end;
  if twin_prime(4) <> -1 then begin
  panic('twin_prime(4) failed');
end;
  if twin_prime(5) <> 7 then begin
  panic('twin_prime(5) failed');
end;
  if twin_prime(17) <> 19 then begin
  panic('twin_prime(17) failed');
end;
  if twin_prime(0) <> -1 then begin
  panic('twin_prime(0) failed');
end;
end;
procedure main();
begin
  test_twin_prime();
  writeln(twin_prime(3));
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
