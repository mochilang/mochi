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
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function largest_prime_factor(largest_prime_factor_n: integer): integer; forward;
procedure main(); forward;
function largest_prime_factor(largest_prime_factor_n: integer): integer;
var
  largest_prime_factor_num: integer;
  largest_prime_factor_prime: integer;
  largest_prime_factor_i: integer;
begin
  if largest_prime_factor_n <= 0 then begin
  panic('Parameter n must be greater than or equal to one.');
end;
  largest_prime_factor_num := largest_prime_factor_n;
  largest_prime_factor_prime := 1;
  largest_prime_factor_i := 2;
  while (largest_prime_factor_i * largest_prime_factor_i) <= largest_prime_factor_num do begin
  while (largest_prime_factor_num mod largest_prime_factor_i) = 0 do begin
  largest_prime_factor_prime := largest_prime_factor_i;
  largest_prime_factor_num := largest_prime_factor_num div largest_prime_factor_i;
end;
  largest_prime_factor_i := largest_prime_factor_i + 1;
end;
  if largest_prime_factor_num > 1 then begin
  largest_prime_factor_prime := largest_prime_factor_num;
end;
  exit(largest_prime_factor_prime);
end;
procedure main();
begin
  writeln(IntToStr(largest_prime_factor(600851475143)));
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
