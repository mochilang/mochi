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
  n: integer;
  num: integer;
function isqrt(n: integer): integer; forward;
function prime_sieve(num: integer): IntArray; forward;
function isqrt(n: integer): integer;
var
  isqrt_r: integer;
begin
  isqrt_r := 0;
  while ((isqrt_r + 1) * (isqrt_r + 1)) <= n do begin
  isqrt_r := isqrt_r + 1;
end;
  exit(isqrt_r);
end;
function prime_sieve(num: integer): IntArray;
var
  prime_sieve_sieve: array of boolean;
  prime_sieve_i: integer;
  prime_sieve_prime: array of integer;
  prime_sieve_start: integer;
  prime_sieve_end_: integer;
  prime_sieve_j: integer;
  prime_sieve_k: integer;
begin
  if num <= 0 then begin
  panic('Invalid input, please enter a positive integer.');
end;
  prime_sieve_sieve := [];
  prime_sieve_i := 0;
  while prime_sieve_i <= num do begin
  prime_sieve_sieve := concat(prime_sieve_sieve, [true]);
  prime_sieve_i := prime_sieve_i + 1;
end;
  prime_sieve_prime := [];
  prime_sieve_start := 2;
  prime_sieve_end_ := isqrt(num);
  while prime_sieve_start <= prime_sieve_end_ do begin
  if prime_sieve_sieve[prime_sieve_start] then begin
  prime_sieve_prime := concat(prime_sieve_prime, IntArray([prime_sieve_start]));
  prime_sieve_j := prime_sieve_start * prime_sieve_start;
  while prime_sieve_j <= num do begin
  if prime_sieve_sieve[prime_sieve_j] then begin
  prime_sieve_sieve[prime_sieve_j] := false;
end;
  prime_sieve_j := prime_sieve_j + prime_sieve_start;
end;
end;
  prime_sieve_start := prime_sieve_start + 1;
end;
  prime_sieve_k := prime_sieve_end_ + 1;
  while prime_sieve_k <= num do begin
  if prime_sieve_sieve[prime_sieve_k] then begin
  prime_sieve_prime := concat(prime_sieve_prime, IntArray([prime_sieve_k]));
end;
  prime_sieve_k := prime_sieve_k + 1;
end;
  exit(prime_sieve_prime);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(prime_sieve(50)));
  writeln(list_int_to_str(prime_sieve(25)));
  writeln(list_int_to_str(prime_sieve(10)));
  writeln(list_int_to_str(prime_sieve(9)));
  writeln(list_int_to_str(prime_sieve(2)));
  writeln(list_int_to_str(prime_sieve(1)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
