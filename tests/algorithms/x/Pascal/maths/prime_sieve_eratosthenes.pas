{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type BoolArray = array of boolean;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  a: IntArray;
  b: IntArray;
  num: integer;
function prime_sieve_eratosthenes(num: integer): IntArray; forward;
function list_eq(a: IntArray; b: IntArray): boolean; forward;
procedure test_prime_sieve_eratosthenes(); forward;
procedure main(); forward;
function prime_sieve_eratosthenes(num: integer): IntArray;
var
  prime_sieve_eratosthenes_primes: array of boolean;
  prime_sieve_eratosthenes_i: integer;
  prime_sieve_eratosthenes_p: integer;
  prime_sieve_eratosthenes_j: integer;
  prime_sieve_eratosthenes_result_: array of integer;
  prime_sieve_eratosthenes_k: integer;
begin
  if num <= 0 then begin
  panic('Input must be a positive integer');
end;
  prime_sieve_eratosthenes_primes := [];
  prime_sieve_eratosthenes_i := 0;
  while prime_sieve_eratosthenes_i <= num do begin
  prime_sieve_eratosthenes_primes := concat(prime_sieve_eratosthenes_primes, [true]);
  prime_sieve_eratosthenes_i := prime_sieve_eratosthenes_i + 1;
end;
  prime_sieve_eratosthenes_p := 2;
  while (prime_sieve_eratosthenes_p * prime_sieve_eratosthenes_p) <= num do begin
  if prime_sieve_eratosthenes_primes[prime_sieve_eratosthenes_p] then begin
  prime_sieve_eratosthenes_j := prime_sieve_eratosthenes_p * prime_sieve_eratosthenes_p;
  while prime_sieve_eratosthenes_j <= num do begin
  prime_sieve_eratosthenes_primes[prime_sieve_eratosthenes_j] := false;
  prime_sieve_eratosthenes_j := prime_sieve_eratosthenes_j + prime_sieve_eratosthenes_p;
end;
end;
  prime_sieve_eratosthenes_p := prime_sieve_eratosthenes_p + 1;
end;
  prime_sieve_eratosthenes_result_ := [];
  prime_sieve_eratosthenes_k := 2;
  while prime_sieve_eratosthenes_k <= num do begin
  if prime_sieve_eratosthenes_primes[prime_sieve_eratosthenes_k] then begin
  prime_sieve_eratosthenes_result_ := concat(prime_sieve_eratosthenes_result_, IntArray([prime_sieve_eratosthenes_k]));
end;
  prime_sieve_eratosthenes_k := prime_sieve_eratosthenes_k + 1;
end;
  exit(prime_sieve_eratosthenes_result_);
end;
function list_eq(a: IntArray; b: IntArray): boolean;
var
  list_eq_i: integer;
begin
  if Length(a) <> Length(b) then begin
  exit(false);
end;
  list_eq_i := 0;
  while list_eq_i < Length(a) do begin
  if a[list_eq_i] <> b[list_eq_i] then begin
  exit(false);
end;
  list_eq_i := list_eq_i + 1;
end;
  exit(true);
end;
procedure test_prime_sieve_eratosthenes();
begin
  if not list_eq(prime_sieve_eratosthenes(10), [2, 3, 5, 7]) then begin
  panic('test 10 failed');
end;
  if not list_eq(prime_sieve_eratosthenes(20), [2, 3, 5, 7, 11, 13, 17, 19]) then begin
  panic('test 20 failed');
end;
  if not list_eq(prime_sieve_eratosthenes(2), [2]) then begin
  panic('test 2 failed');
end;
  if Length(prime_sieve_eratosthenes(1)) <> 0 then begin
  panic('test 1 failed');
end;
end;
procedure main();
begin
  test_prime_sieve_eratosthenes();
  writeln(list_int_to_str(prime_sieve_eratosthenes(20)));
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
